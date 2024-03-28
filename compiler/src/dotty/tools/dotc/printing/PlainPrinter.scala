package dotty.tools.dotc
package printing

import core.*
import Texts.*, Types.*, Flags.*, Names.*, Symbols.*, NameOps.*, Constants.*, Denotations.*
import StdNames.*
import Contexts.*
import Scopes.Scope, Denotations.Denotation, Annotations.Annotation
import StdNames.nme
import ast.Trees.*
import typer.Implicits.*
import typer.ImportInfo
import Variances.varianceSign
import util.SourcePosition
import scala.util.control.NonFatal
import scala.annotation.switch
import config.{Config, Feature}
import cc.{CapturingType, RetainingType, CaptureSet, ReachCapability, MaybeCapability, isBoxed, levelOwner, retainedElems, isRetainsLike}

class PlainPrinter(_ctx: Context) extends Printer {

  /** The context of all public methods in Printer and subclasses.
   *  Overridden in RefinedPrinter.
   */
  def printerContext: Context = _ctx.addMode(Mode.Printing)
  protected given [DummyToEnforceDef]: Context = printerContext

  protected def printDebug = ctx.settings.YprintDebug.value

  private var openRecs: List[RecType] = Nil

  protected def maxToTextRecursions: Int = 100

  protected def showUniqueIds = ctx.settings.uniqid.value || Printer.debugPrintUnique
  protected def showNestingLevel = ctx.settings.YprintLevel.value

  protected final def limiter: MessageLimiter = ctx.property(MessageLimiter).get

  protected def controlled(op: => Text): Text = limiter.controlled(op)

  def Str(str: String, lineRange: LineRange = EmptyLineRange): Str =
    limiter.register(str)
    Texts.Str(str, lineRange)

  given stringToText: Conversion[String, Text] = Str(_)

  /** If true, tweak output so it is the same before and after pickling */
  protected def homogenizedView: Boolean = ctx.settings.YtestPickler.value
  protected def debugPos: Boolean = ctx.settings.YdebugPos.value

  def homogenize(tp: Type): Type =
    if (homogenizedView)
      tp match {
        case tp: ThisType if tp.cls.is(Package) && !tp.cls.isEffectiveRoot =>
          requiredPackage(tp.cls.fullName).termRef
        case tp: TypeVar if tp.isInstantiated =>
          homogenize(tp.instanceOpt)
        case AndType(tp1, tp2) =>
          homogenize(tp1) & homogenize(tp2)
        case OrType(tp1, tp2) =>
          homogenize(tp1) | homogenize(tp2)
        case AnnotatedType(parent, annot)
        if !ctx.mode.is(Mode.Type) && annot.symbol == defn.UncheckedVarianceAnnot
           || annot.symbol.isRetainsLike =>
          homogenize(parent)
        case tp: SkolemType =>
          homogenize(tp.info)
        case tp: LazyRef =>
          homogenize(tp.ref)
        case tp @ AppliedType(tycon, args) =>
          if (defn.isCompiletimeAppliedType(tycon.typeSymbol)) tp.tryCompiletimeConstantFold
          else tycon.dealias.appliedTo(args)
        case tp: NamedType =>
          tp.reduceProjection
        case _ =>
          tp
      }
    else tp

  private def sameBound(lo: Type, hi: Type): Boolean =
    try lo frozen_=:= hi catch { case NonFatal(ex) => false }

  private def homogenizeArg(tp: Type) = tp match {
    case TypeBounds(lo, hi) if homogenizedView && sameBound(lo, hi) => homogenize(hi)
    case _ => tp
  }

  private def selfRecName(n: Int) = s"z$n"

  /** If the name of the symbol's owner should be used when you care about
   *  seeing an interesting name: in such cases this symbol is e.g. a method
   *  parameter with a synthetic name, a constructor named "this", an object
   *  "package", etc.  The kind string, if non-empty, will be phrased relative
   *  to the name of the owner.
   */
  protected def hasMeaninglessName(sym: Symbol): Boolean = (
       sym.is(Param) && sym.owner.isSetter    // x$1
    || sym.isClassConstructor                  // this
    || (sym.name == nme.PACKAGE)               // package
  )

  def nameString(name: Name): String =
    if (name eq tpnme.FromJavaObject) && !printDebug
    then nameString(tpnme.Object)
    else name.toString

  def toText(name: Name): Text = Str(nameString(name))

  /** String representation of a name used in a refinement
   *  In refined printing this undoes type parameter expansion
   */
  protected def refinementNameString(tp: RefinedType): String = nameString(tp.refinedName)

  /** String representation of a refinement */
  protected def toTextRefinement(rt: RefinedType): Text =
    val keyword = rt.refinedInfo match {
      case _: ExprType | _: MethodOrPoly => "def "
      case _: TypeBounds => "type "
      case _: TypeProxy => "val "
      case _ => ""
    }
    (keyword ~ refinementNameString(rt) ~ toTextRHS(rt.refinedInfo)).close

  protected def argText(arg: Type, isErased: Boolean = false): Text = keywordText("erased ").provided(isErased) ~ (homogenizeArg(arg) match {
    case arg: TypeBounds => "?" ~ toText(arg)
    case arg => toText(arg)
  })

  /** Pretty-print comma-separated type arguments for a constructor to be inserted among parentheses or brackets
    * (hence with `GlobalPrec` precedence).
    */
  protected def argsText(args: List[Type]): Text =
    atPrec(GlobalPrec) { Text(args.map(arg => argText(arg) ), ", ") }

  /** The longest sequence of refinement types, starting at given type
   *  and following parents.
   */
  private def refinementChain(tp: Type): List[Type] =
    tp :: (tp match {
      case tp: RefinedType => refinementChain(tp.parent.stripTypeVar)
      case _ => Nil
    })

  /** Direct references to these symbols are printed without their prefix for convenience.
   *  They are either aliased in scala.Predef or in the scala package object, as well as `Object`
   */
  private lazy val printWithoutPrefix: Set[Symbol] =
    (defn.ScalaPredefModule.termRef.typeAliasMembers
      ++ defn.ScalaPackageObject.termRef.typeAliasMembers).map(_.info.classSymbol).toSet
    + defn.ObjectClass
    + defn.FromJavaObjectSymbol

  def toTextCaptureSet(cs: CaptureSet): Text =
    if printDebug && ctx.settings.YccDebug.value && !cs.isConst then cs.toString
    else if cs == CaptureSet.Fluid then "<fluid>"
    else
      val core: Text =
        if !cs.isConst && cs.elems.isEmpty then "?"
        else "{" ~ Text(cs.elems.toList.map(toTextCaptureRef), ", ") ~ "}"
           //     ~ Str("?").provided(!cs.isConst)
      core ~ cs.optionalInfo

  private def toTextRetainedElem[T <: Untyped](ref: Tree[T]): Text = ref match
    case ref: RefTree[?] if ref.typeOpt.exists =>
      toTextCaptureRef(ref.typeOpt)
    case _ =>
      toText(ref)

  private def toTextRetainedElems[T <: Untyped](refs: List[Tree[T]]): Text =
    "{" ~ Text(refs.map(ref => toTextRetainedElem(ref)), ", ") ~ "}"

  /** Print capturing type, overridden in RefinedPrinter to account for
   *  capturing function types.
   */
  protected def toTextCapturing(parent: Type, refsText: Text, boxText: Text): Text =
    changePrec(InfixPrec):
      boxText ~ toTextLocal(parent) ~ "^"
      ~ (refsText provided refsText != rootSetText)

  final protected def rootSetText = Str("{cap}") // TODO Use disambiguation

  def toText(tp: Type): Text = controlled {
    homogenize(tp) match {
      case tp: TypeType =>
        toTextRHS(tp)
      case tp: TermRef
      if !tp.denotationIsCurrent
          && !homogenizedView // always print underlying when testing picklers
          && !tp.isRootCapability
          || tp.symbol.is(Module)
          || tp.symbol.name == nme.IMPORT =>
        toTextRef(tp) ~ ".type"
      case tp: TermRef if tp.denot.isOverloaded =>
        "<overloaded " ~ toTextRef(tp) ~ ">"
      case tp: TypeRef =>
        if (printWithoutPrefix.contains(tp.symbol))
          selectionString(tp)
        else
          toTextPrefixOf(tp) ~ selectionString(tp)
      case tp: TermParamRef =>
        ParamRefNameString(tp) ~ lambdaHash(tp.binder) ~ ".type"
      case tp: TypeParamRef =>
        val suffix =
          if showNestingLevel then
            val tvar = ctx.typerState.constraint.typeVarOfParam(tp)
            if tvar.exists then s"#${tvar.asInstanceOf[TypeVar].nestingLevel.toString}" else ""
          else ""
        ParamRefNameString(tp) ~ lambdaHash(tp.binder) ~ suffix
      case tp: SingletonType =>
        toTextSingleton(tp)
      case AppliedType(tycon, args) =>
        (toTextLocal(tycon) ~ "[" ~ argsText(args) ~ "]").close
      case tp: RefinedType =>
        val parent :: (refined: List[RefinedType @unchecked]) =
          refinementChain(tp).reverse: @unchecked
        toTextLocal(parent) ~ "{" ~ Text(refined map toTextRefinement, "; ").close ~ "}"
      case tp: RecType =>
        try {
          openRecs = tp :: openRecs
          "{" ~ selfRecName(openRecs.length) ~ " => " ~ toTextGlobal(tp.parent) ~ "}"
        }
        finally openRecs = openRecs.tail
      case AndType(tp1, tp2) =>
        changePrec(AndTypePrec) { toText(tp1) ~ " & " ~ atPrec(AndTypePrec + 1) { toText(tp2) } }
      case OrType(tp1, tp2) =>
        changePrec(OrTypePrec) { toText(tp1) ~ " | " ~ atPrec(OrTypePrec + 1) { toText(tp2) } }
      case MatchType(bound, scrutinee, cases) =>
        changePrec(GlobalPrec) {
          def caseText(tp: Type): Text = tp match {
            case tp: HKTypeLambda => caseText(tp.resultType)
            case defn.MatchCase(pat, body) => "case " ~ toText(pat) ~ " => " ~ toText(body)
            case _ => "case " ~ toText(tp)
          }
          def casesText = Text(cases.map(caseText), "\n")
          atPrec(InfixPrec) { toText(scrutinee) } ~
          keywordStr(" match ") ~ "{" ~ casesText ~ "}" ~
          (" <: " ~ toText(bound) provided !bound.isAny)
        }.close
      case tp @ CapturingType(parent, refs) =>
        val boxText: Text = Str("box ") provided tp.isBoxed //&& ctx.settings.YccDebug.value
        val showAsCap = refs.isUniversal && (refs.elems.size == 1 || !printDebug)
        val refsText = if showAsCap then rootSetText else toTextCaptureSet(refs)
        toTextCapturing(parent, refsText, boxText)
      case tp @ RetainingType(parent, refs) =>
        if Feature.ccEnabledSomewhere then
          val refsText = refs match
            case ref :: Nil if ref.symbol == defn.captureRoot => rootSetText
            case _ => toTextRetainedElems(refs)
          toTextCapturing(parent, refsText, "") ~ Str("R").provided(printDebug)
        else toText(parent)
      case tp: PreviousErrorType if ctx.settings.XprintTypes.value =>
        "<error>" // do not print previously reported error message because they may try to print this error type again recuresevely
      case tp: ErrorType =>
        s"<error ${tp.msg.message}>"
      case tp: WildcardType =>
        if (tp.optBounds.exists) "<?" ~ toTextRHS(tp.bounds) ~ ">" else "<?>"
      case NoType =>
        "<notype>"
      case NoPrefix =>
        "<noprefix>"
      case tp: MethodType =>
        changePrec(GlobalPrec) {
          "("
          ~ keywordText("using ").provided(tp.isContextualMethod)
          ~ keywordText("implicit ").provided(tp.isImplicitMethod && !tp.isContextualMethod)
          ~ paramsText(tp)
          ~ ")"
          ~ (Str(": ") provided !tp.resultType.isInstanceOf[MethodOrPoly])
          ~ toText(tp.resultType)
        }
      case ExprType(restp) =>
        def arrowText: Text = restp match
          case AnnotatedType(parent, ann) if ann.symbol == defn.RetainsByNameAnnot =>
            val refs = ann.tree.retainedElems
            if refs.exists(_.symbol == defn.captureRoot) then Str("=>")
            else Str("->") ~ toTextRetainedElems(refs)
          case _ =>
            if Feature.pureFunsEnabled then "->" else "=>"
        changePrec(GlobalPrec)(arrowText ~ " " ~ toText(restp))
      case tp: HKTypeLambda =>
        changePrec(GlobalPrec) {
          "[" ~ paramsText(tp) ~ "]" ~ lambdaHash(tp) ~ Str(" =>> ") ~ toTextGlobal(tp.resultType)
        }
      case tp: PolyType =>
        changePrec(GlobalPrec) {
          "[" ~ paramsText(tp) ~ "]" ~ lambdaHash(tp) ~
          (Str(": ") provided !tp.resultType.isInstanceOf[MethodOrPoly]) ~
          toTextGlobal(tp.resultType)
        }
      case AnnotatedType(tpe, annot) =>
        if annot.symbol == defn.InlineParamAnnot || annot.symbol == defn.ErasedParamAnnot
        then toText(tpe)
        else if (annot.symbol == defn.IntoAnnot || annot.symbol == defn.IntoParamAnnot)
            && !printDebug
        then atPrec(GlobalPrec)( Str("into ") ~ toText(tpe) )
        else toTextLocal(tpe) ~ " " ~ toText(annot)
      case FlexibleType(_, tpe) =>
        "(" ~ toText(tpe) ~ ")?"
      case tp: TypeVar =>
        def toTextCaret(tp: Type) = if printDebug then toTextLocal(tp) ~ Str("^") else toText(tp)
        if (tp.isInstantiated)
          toTextCaret(tp.instanceOpt)
        else {
          val constr = ctx.typerState.constraint
          val bounds =
            if constr.contains(tp) then
              withMode(Mode.Printing)(TypeComparer.fullBounds(tp.origin))
            else
              TypeBounds.empty
          if (bounds.isTypeAlias) toTextCaret(bounds.lo)
          else if (ctx.settings.YshowVarBounds.value) "(" ~ toText(tp.origin) ~ "?" ~ toText(bounds) ~ ")"
          else toText(tp.origin)
        }
      case tp: LazyRef =>
        def refTxt =
          try toTextGlobal(tp.ref)
          catch {
            case ex: Throwable => Str("...")
          }
        "LazyRef(" ~ refTxt ~ ")"
      case Range(lo, hi) =>
        toText(lo) ~ ".." ~ toText(hi)
      case _ =>
        tp.fallbackToText(this)
    }
  }.close

  def toTextSingleton(tp: SingletonType): Text =
    "(" ~ toTextRef(tp) ~ " : " ~ toTextGlobal(tp.underlying) ~ ")"

  protected def paramsText(lam: LambdaType): Text = {
    def paramText(ref: ParamRef) =
      val erased = ref.underlying.hasAnnotation(defn.ErasedParamAnnot)
      keywordText("erased ").provided(erased) ~ ParamRefNameString(ref) ~ lambdaHash(lam) ~ toTextRHS(ref.underlying, isParameter = true)
    Text(lam.paramRefs.map(paramText), ", ")
  }

  protected def ParamRefNameString(name: Name): String = nameString(name)

  protected def ParamRefNameString(param: ParamRef): String =
    ParamRefNameString(param.binder.paramNames(param.paramNum))

  /** The name of the symbol without a unique id. */
  protected def simpleNameString(sym: Symbol): String = nameString(sym.name)

  /** If -uniqid is set, the hashcode of the lambda type, after a # */
  protected def lambdaHash(pt: LambdaType): Text =
    if (showUniqueIds)
      try "#" + pt.hashCode
      catch { case ex: NullPointerException => "" }
    else ""

  /** A string to append to a symbol composed of:
   *  - if -uniqid is set, its unique id after a #.
   *  - if -Yprint-level, its nesting level after a %.
   */
  protected def idString(sym: Symbol): String =
    (if (showUniqueIds || Printer.debugPrintUnique) "#" + sym.id else "") +
    (if showNestingLevel then "%" + sym.nestingLevel else "")

  def nameString(sym: Symbol): String =
    simpleNameString(sym) + idString(sym) // + "<" + (if (sym.exists) sym.owner else "") + ">"

  def fullNameString(sym: Symbol): String =
    if (sym eq defn.FromJavaObjectSymbol) && !printDebug then
      fullNameString(defn.ObjectClass)
    else if sym.isRoot || sym == NoSymbol || sym.owner.isEffectiveRoot then
      nameString(sym)
    else
      fullNameString(fullNameOwner(sym)) + "." + nameString(sym)

  protected def fullNameOwner(sym: Symbol): Symbol = sym.effectiveOwner.enclosingClass

  protected def objectPrefix: String = "object "
  protected def packagePrefix: String = "package "

  protected def trimPrefix(text: Text): Text =
    text.stripPrefix(objectPrefix).stripPrefix(packagePrefix)

  protected def selectionString(tp: NamedType): String = {
    val sym = if (homogenizedView) tp.symbol else tp.currentSymbol
    if (sym.exists) nameString(sym) else nameString(tp.name)
  }

  /** The string representation of this type used as a prefix */
  def toTextRef(tp: SingletonType): Text = controlled {
    tp match {
      case tp: TermRef =>
        toTextPrefixOf(tp) ~ selectionString(tp)
      case tp: ThisType =>
        nameString(tp.cls) + ".this"
      case SuperType(thistpe: SingletonType, _) =>
        toTextRef(thistpe).map(_.replaceAll("""\bthis$""", "super").nn)
      case SuperType(thistpe, _) =>
        "Super(" ~ toTextGlobal(thistpe) ~ ")"
      case tp @ ConstantType(value) =>
        toText(value)
      case pref: TermParamRef =>
        ParamRefNameString(pref) ~ lambdaHash(pref.binder)
      case tp: RecThis =>
        val idx = openRecs.reverse.indexOf(tp.binder)
        if (idx >= 0) selfRecName(idx + 1)
        else "{...}.this" // TODO move underlying type to an addendum, e.g. ... z3 ... where z3: ...
      case tp: SkolemType =>
        if (homogenizedView) toText(tp.info)
        else if (ctx.settings.XprintTypes.value) "<" ~ toText(tp.repr) ~ ":" ~ toText(tp.info) ~ ">"
        else toText(tp.repr)
    }
  }

  def toTextCaptureRef(tp: Type): Text =
    homogenize(tp) match
      case tp: TermRef if tp.symbol == defn.captureRoot => Str("cap")
      case tp: SingletonType => toTextRef(tp)
      case ReachCapability(tp1) => toTextRef(tp1) ~ "*"
      case MaybeCapability(tp1) => toTextRef(tp1) ~ "?"
      case _ => toText(tp)

  protected def isOmittablePrefix(sym: Symbol): Boolean =
    defn.unqualifiedOwnerTypes.exists(_.symbol == sym) || isEmptyPrefix(sym)

  /** The string representation of type prefix, including separator */
  def toTextPrefixOf(tp: NamedType): Text = controlled {
      homogenize(tp.prefix) match {
        case NoPrefix => ""
        case tp: SingletonType => toTextRef(tp) ~ "."
        case tp => trimPrefix(toTextLocal(tp)) ~ "#"
      }
  }

  protected def isEmptyPrefix(sym: Symbol): Boolean =
    sym.isEffectiveRoot || sym.isAnonymousClass || sym.name.isReplWrapperName

  /** String representation of a definition's type following its name,
   *  if symbol is completed, "?" otherwise.
   */
  protected def toTextRHS(optType: Option[Type]): Text = optType match {
    case Some(tp) => toTextRHS(tp)
    case None => "?"
  }

  protected def decomposeLambdas(bounds: TypeBounds): (Text, TypeBounds) =
    def decompose(tp: Type): (Text, Type) = tp.stripTypeVar match
      case lam: HKTypeLambda =>
        val names =
          if lam.isDeclaredVarianceLambda then
            lam.paramNames.lazyZip(lam.declaredVariances).map((name, v) =>
              varianceSign(v) + name)
          else lam.paramNames.map(_.toString)
        val infos = lam.paramInfos.map(toText)
        val tparams = names.zip(infos).map(_ ~ _)
        ("[" ~ Text(tparams, ",") ~ "]", lam.resType)
      case _ =>
        ("", tp)
    bounds match
      case bounds: AliasingBounds =>
        val (tparamStr, aliasRhs) = decompose(bounds.alias)
        (tparamStr, bounds.derivedAlias(aliasRhs))
      case TypeBounds(lo, hi) =>
        val (_, loRhs) = decompose(lo)
        val (tparamStr, hiRhs) = decompose(hi)
        (tparamStr, bounds.derivedTypeBounds(loRhs, hiRhs))
  end decomposeLambdas

  /** String representation of a definition's type following its name */
  protected def toTextRHS(tp: Type, isParameter: Boolean = false): Text = controlled {
    homogenize(tp) match {
      case tp: TypeBounds =>
        val (tparamStr, rhs) = decomposeLambdas(tp)
        val binder = rhs match
          case tp: AliasingBounds =>
            " = " ~ toText(tp.alias)
          case TypeBounds(lo, hi) =>
            (if lo.isExactlyNothing then Text() else " >: " ~ toText(lo))
            ~ (if hi.isExactlyAny || (!printDebug && hi.isFromJavaObject) then Text() else " <: " ~ toText(hi))
        tparamStr ~ binder
      case tp @ ClassInfo(pre, cls, cparents, decls, selfInfo) =>
        val preText = toTextLocal(pre)
        val (tparams, otherDecls) = decls.toList partition treatAsTypeParam
        val tparamsText =
          if (tparams.isEmpty) Text() else ("[" ~ dclsText(tparams) ~ "]").close
        val selfText: Text = selfInfo match {
          case NoType => Text()
          case sym: Symbol if !sym.isCompleted => "this: ? =>"
          case _ => "this: " ~ atPrec(InfixPrec) { toText(tp.selfType) } ~ " =>"
        }
        val trueDecls = otherDecls.filterNot(treatAsTypeArg)
        val declsText =
          if (trueDecls.isEmpty || !ctx.settings.Ydebug.value) Text()
          else dclsText(trueDecls)
        tparamsText ~ " extends " ~ toTextParents(tp.parents) ~~ "{" ~ selfText ~ declsText ~
          "} at " ~ preText
      case mt: MethodType =>
        toTextGlobal(mt)
      case tp: ExprType =>
        // parameterless methods require special treatment, see #11201
        (if (isParameter) ": => " else ": ") ~ toTextGlobal(tp.widenExpr)
      case tp: PolyType =>
        "[" ~ paramsText(tp) ~ "]"
        ~ (Str(": ") provided !tp.resultType.isInstanceOf[MethodOrPoly])
        ~ toTextGlobal(tp.resultType)
      case tp =>
        ": " ~ toTextGlobal(tp)
    }
  }

  protected def toTextParents(parents: List[Type]): Text = Text(parents.map(toTextLocal), " with ")

  protected def treatAsTypeParam(sym: Symbol): Boolean = false
  protected def treatAsTypeArg(sym: Symbol): Boolean = false

  /** String representation of symbol's kind. */
  def kindString(sym: Symbol): String = {
    val flags = sym.flagsUNSAFE
    if (flags.is(PackageClass)) "package class"
    else if (flags.is(PackageVal)) "package"
    else if (sym.isPackageObject)
      if (sym.isClass) "package object class"
      else "package object"
    else if (sym.isAnonymousClass) "anonymous class"
    else if (flags.is(ModuleClass)) "object class"
    else if (flags.is(ModuleVal)) "object"
    else if (flags.is(Trait)) "trait"
    else if (sym.isClass) "class"
    else if (sym.isType) "type"
    else if (sym.isGetter) "getter"
    else if (sym.isSetter) "setter"
    else if sym.is(Param) then "parameter"
    else if sym.is(Given) then "given instance"
    else if (flags.is(Lazy)) "lazy value"
    else if (flags.is(Mutable)) "variable"
    else if (sym.isClassConstructor && sym.isPrimaryConstructor) "primary constructor"
    else if (sym.isClassConstructor) "constructor"
    else if (sym.is(Method)) "method"
    else if (sym.isTerm) "value"
    else ""
  }

  /** String representation of symbol's definition keyword */
  protected def keyString(sym: Symbol): String = {
    val flags = sym.flagsUNSAFE
    if (flags.isAllOf(JavaInterface)) "interface"
    else if (flags.is(Trait)) "trait"
    else if (flags.is(Module)) "object"
    else if (sym.isClass) "class"
    else if (sym.isType) "type"
    else if (flags.is(Mutable)) "var"
    else if (flags.is(Package)) "package"
    else if (sym.is(Method)) "def"
    else if (sym.isTerm && !flags.is(Param)) "val"
    else ""
  }

  protected def privateWithinString(sym: Symbol): String =
    if (sym.exists && sym.privateWithin.exists)
      nameString(sym.privateWithin.name.stripModuleClassSuffix)
    else ""

  /** String representation of symbol's flags */
  protected def toTextFlags(sym: Symbol): Text = toTextFlags(sym, sym.flagsUNSAFE)

  protected def toTextFlags(sym: Symbol, flags: FlagSet): Text =
    Text(flags.flagStrings(privateWithinString(sym)).map(flag => stringToText(keywordStr(flag))), " ")

  def annotsText(sym: Symbol): Text = Text(sym.annotations.map(toText))

  def dclText(sym: Symbol): Text = dclTextWithInfo(sym, sym.unforcedInfo)

  def dclText(d: SingleDenotation): Text = dclTextWithInfo(d.symbol, Some(d.info))

  private def dclTextWithInfo(sym: Symbol, info: Option[Type]): Text =
    (toTextFlags(sym) ~~ keyString(sym) ~~
      (varianceSign(sym.variance) ~ nameString(sym)) ~ toTextRHS(info)).close

  def toText(sym: Symbol): Text =
    (kindString(sym) ~~ {
      if (sym.isAnonymousClass) toTextParents(sym.info.parents) ~~ "{...}"
      else if (hasMeaninglessName(sym) && !printDebug) simpleNameString(sym.owner) + idString(sym)
      else if sym.is(Package) then fullNameString(sym)
      else nameString(sym)
    }).close

  def locationText(sym: Symbol): Text =
    if (!sym.exists) ""
    else {
      val ownr = sym.effectiveOwner
      if (ownr.isClass && !isEmptyPrefix(ownr)) " in " ~ toText(ownr) else Text()
    }

  def locatedText(sym: Symbol): Text =
    (toText(sym) ~ locationText(sym)).close

  def extendedLocationText(sym: Symbol): Text =
    if (!sym.exists) ""
    else if isEmptyPrefix(sym.owner) then
      " in the empty package"
    else {
      def recur(ownr: Symbol, innerLocation: String): Text = {
        def nextOuter(innerKind: String): Text =
          recur(ownr.effectiveOwner,
            if (!innerLocation.isEmpty) innerLocation
            else s" in an anonymous $innerKind")
        def showLocation(ownr: Symbol, where: String): Text =
          innerLocation ~ " " ~ where ~ " " ~ toText(ownr)
        if (ownr.isAnonymousClass) nextOuter("class")
        else if (ownr.isAnonymousFunction) nextOuter("function")
        else if (isEmptyPrefix(ownr)) ""
        else if (ownr.isLocalDummy) showLocation(ownr.owner, "locally defined in")
        else if (ownr.isTerm && !ownr.isOneOf(Module | Method)) showLocation(ownr, "in the initializer of")
        else showLocation(ownr, "in")
      }
      recur(sym.owner, "")
    }

  def toText(denot: Denotation): Text = toText(denot.symbol) ~ "/D"

  private def escapedChar(ch: Char): String = (ch: @switch) match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"' => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _ => if ch.isControl then f"${"\\"}u${ch.toInt}%04x" else String.valueOf(ch).nn
  }

  def toText(const: Constant): Text = const.tag match {
    case StringTag => stringText("\"" + escapedString(const.value.toString) + "\"")
    case ClazzTag => "classOf[" ~ toText(const.typeValue) ~ "]"
    case CharTag => literalText(s"'${escapedChar(const.charValue)}'")
    case LongTag => literalText(const.longValue.toString + "L")
    case DoubleTag => literalText(const.doubleValue.toString + "d")
    case FloatTag => literalText(const.floatValue.toString + "f")
    case _ => literalText(String.valueOf(const.value).nn)
  }

  /** Usual target for `Annotation#toText`, overridden in RefinedPrinter */
  def annotText(annot: Annotation): Text = s"@${annot.symbol.name}"

  def toText(annot: Annotation): Text = annot.toText(this)

  def toText(param: LambdaParam): Text =
    varianceSign(param.paramVariance)
    ~ toText(param.paramName)
    ~ (if param.isTypeParam then "" else ": ")
    ~ toText(param.paramInfo)

  protected def escapedString(str: String): String = str flatMap escapedChar

  def dclsText(syms: List[Symbol], sep: String): Text = Text(syms map dclText, sep)

  def toText(sc: Scope): Text =
    ("Scope{" ~ dclsText(sc.toList) ~ "}").close

  def toText[T <: Untyped](tree: Tree[T]): Text = {
    def toTextElem(elem: Any): Text = elem match {
      case elem: Showable => elem.toText(this)
      case elem: List[?] => "List(" ~ Text(elem map toTextElem, ",") ~ ")"
      case elem => elem.toString
    }
    val nodeName = tree.productPrefix
    val elems =
      Text(tree.productIterator.map(toTextElem).toList, ", ")
    val tpSuffix =
      if (ctx.settings.XprintTypes.value && tree.hasType)
        " | " ~ toText(tree.typeOpt)
      else
        Text()

    nodeName ~ "(" ~ elems ~ tpSuffix ~ ")" ~ (Str(tree.sourcePos.toString) provided printDebug)
  }.close

  def toText(pos: SourcePosition): Text =
    if (!pos.exists) "<no position>"
    else if (pos.source.exists) s"${pos.source.file.name}:${pos.line + 1}"
    else s"(no source file, offset = ${pos.span.point})"

  def toText(cand: Candidate): Text =
    "Cand("
      ~ toTextRef(cand.ref)
      ~ (if cand.isConversion then " conv" else "")
      ~ (if cand.isExtension then " ext" else "")
      ~ Str(" L" + cand.level) ~ ")"

  def toText(result: SearchResult): Text = result match {
    case result: SearchSuccess =>
      "SearchSuccess: " ~ toText(result.ref) ~ " via " ~ toText(result.tree)
    case result: SearchFailure =>
      result.reason match {
        case _: NoMatchingImplicits => "No Matching Implicit"
        case _: DivergingImplicit => "Diverging Implicit"
        case result: AmbiguousImplicits =>
          "Ambiguous Implicit: " ~ toText(result.alt1.ref) ~ " and " ~ toText(result.alt2.ref)
        case _ =>
          "Search Failure: " ~ toText(result.tree)
      }
  }

  def toText(importInfo: ImportInfo): Text =
    val siteStr = importInfo.site.show
    val exprStr = if siteStr.endsWith(".type") then siteStr.dropRight(5) else siteStr
    val selectorStr = importInfo.selectors match
      case sel :: Nil if sel.renamed.isEmpty && sel.bound.isEmpty =>
        if sel.isGiven then "given" else sel.name.show
      case _ => "{...}"
    s"import $exprStr.$selectorStr"

  def toText(c: OrderingConstraint): Text =
    val savedConstraint = ctx.typerState.constraint
    try
      // The current TyperState constraint determines how type variables are printed
      ctx.typerState.constraint = c
      def entryText(tp: Type) = tp match {
        case tp: TypeBounds =>
          toText(tp)
        case _ =>
          " := " ~ toText(tp)
      }
      val indent = 3
      val uninstVarsText = " uninstantiated variables: " ~
        Text(c.uninstVars.map(toText), ", ")
      val constrainedText =
        " constrained types: " ~ Text(c.domainLambdas.map(toText), ", ")
      val boundsText =
        " bounds: " ~ {
          val assocs =
            for (param <- c.domainParams)
            yield (" " * indent) ~ toText(param) ~ entryText(c.entry(param))
          Text(assocs, "\n")
        }
      val orderingText =
        " ordering: " ~ {
          val deps =
            for {
              param <- c.domainParams
              ups = c.minUpper(param)
              if ups.nonEmpty
            }
            yield
              (" " * indent) ~ toText(param) ~ " <: " ~
                Text(ups.map(toText), ", ")
          Text(deps, "\n")
        }
      val depsText = if Config.showConstraintDeps then c.depsToString else ""
      //Printer.debugPrintUnique = false
      Text.lines(List(uninstVarsText, constrainedText, boundsText, orderingText, depsText))
    finally
      ctx.typerState.constraint = savedConstraint

  def toText(g: GadtConstraint): Text =
    val deps = for sym <- g.symbols yield
      val bound = g.fullBounds(sym).nn
      (typeText(toText(sym.typeRef)) ~ toText(bound)).close
    ("GadtConstraint(" ~ Text(deps, ", ") ~ ")").close

  def plain: PlainPrinter = this

  protected def keywordStr(text: String): String = coloredStr(text, SyntaxHighlighting.KeywordColor)
  protected def keywordText(text: String): Text = coloredStr(text, SyntaxHighlighting.KeywordColor)
  protected def valDefText(text: Text): Text = coloredText(text, SyntaxHighlighting.ValDefColor)
  protected def typeText(text: Text): Text = coloredText(text, SyntaxHighlighting.TypeColor)
  protected def literalText(text: Text): Text = coloredText(text, SyntaxHighlighting.LiteralColor)
  protected def stringText(text: Text): Text = coloredText(text, SyntaxHighlighting.StringColor)

  protected def coloredStr(text: String, color: String): String =
    if (ctx.useColors) color + text + SyntaxHighlighting.NoColor else text
  protected def coloredText(text: Text, color: String): Text =
    if (ctx.useColors) color ~ text ~ SyntaxHighlighting.NoColor else text
}

