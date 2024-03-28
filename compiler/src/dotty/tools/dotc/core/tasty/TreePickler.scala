package dotty.tools
package dotc
package core
package tasty

import scala.language.unsafeNulls

import dotty.tools.tasty.TastyFormat.*
import dotty.tools.tasty.TastyBuffer.*

import ast.Trees.*
import ast.{untpd, tpd}
import Contexts.*, Symbols.*, Types.*, Names.*, Constants.*, Decorators.*, Annotations.*, Flags.*
import Comments.{Comment, docCtx}
import NameKinds.*
import StdNames.{nme, tpnme}
import config.Config
import collection.mutable
import reporting.{Profile, NoProfile}
import dotty.tools.tasty.TastyFormat.ASTsSection
import quoted.QuotePatterns

object TreePickler:
  class StackSizeExceeded(val mdef: tpd.MemberDef) extends Exception

class TreePickler(pickler: TastyPickler, attributes: Attributes) {
  val buf: TreeBuffer = new TreeBuffer
  pickler.newSection(ASTsSection, buf)
  import buf.*
  import pickler.nameBuffer.nameIndex
  import tpd.*
  import TreePickler.*

  private val symRefs = Symbols.MutableSymbolMap[Addr](256)
  private val forwardSymRefs = Symbols.MutableSymbolMap[List[Addr]]()
  private val pickledTypes = util.EqHashMap[Type, Addr]()

  /** A list of annotation trees for every member definition, so that later
   *  parallel position pickling does not need to access and force symbols.
   */
  private val annotTrees = util.EqHashMap[untpd.MemberDef, mutable.ListBuffer[Tree]]()

  /** A map from member definitions to their doc comments, so that later
   *  parallel comment pickling does not need to access symbols of trees (which
   *  would involve accessing symbols of named types and possibly changing phases
   *  in doing so).
   */
  private val docStrings = util.EqHashMap[untpd.MemberDef, Comment]()

  private var profile: Profile = NoProfile

  private val isOutlinePickle: Boolean = attributes.isOutline
  private val isJavaPickle: Boolean = attributes.isJava

  def treeAnnots(tree: untpd.MemberDef): List[Tree] =
    val ts = annotTrees.lookup(tree)
    if ts == null then Nil else ts.toList

  def docString(tree: untpd.MemberDef): Option[Comment] =
    Option(docStrings.lookup(tree))

  private inline def withLength(inline op: Unit) = {
    val lengthAddr = reserveRef(relative = true)
    op
    fillRef(lengthAddr, currentAddr, relative = true)
  }

  def addrOfSym(sym: Symbol): Option[Addr] =
    symRefs.get(sym)

  def preRegister(tree: Tree)(using Context): Unit = tree match {
    case tree: MemberDef =>
      if (!symRefs.contains(tree.symbol)) symRefs(tree.symbol) = NoAddr
    case _ =>
  }

  def registerDef(sym: Symbol): Unit =
    symRefs(sym) = currentAddr
    val refs = forwardSymRefs.lookup(sym)
    if refs != null then
      refs.foreach(fillRef(_, currentAddr, relative = false))
      forwardSymRefs -= sym

  def pickleName(name: Name): Unit = writeNat(nameIndex(name).index)

  private def pickleNameAndSig(name: Name, sig: Signature, target: Name): Unit =
    pickleName(
      if (sig eq Signature.NotAMethod) name
      else SignedName(name.toTermName, sig, target.asTermName))

  private def pickleSymRef(sym: Symbol)(using Context) =
    val label: Addr | Null = symRefs.lookup(sym)
    if label == null then
      // See pos/t1957.scala for an example where this can happen.
      // I believe it's a bug in typer: the type of an implicit argument refers
      // to a closure parameter outside the closure itself. TODO: track this down, so that we
      // can eliminate this case.
      report.log(i"pickling reference to as yet undefined $sym in ${sym.owner}", sym.srcPos)
      pickleForwardSymRef(sym)
    else if label == NoAddr then
      pickleForwardSymRef(sym)
    else
      writeRef(label.uncheckedNN) // !!! Dotty problem: Not clear why nn or uncheckedNN is needed here

  private def pickleForwardSymRef(sym: Symbol)(using Context) = {
    val ref = reserveRef(relative = false)
    assert(!sym.is(Flags.Package), sym)
    forwardSymRefs(sym) = ref :: forwardSymRefs.getOrElse(sym, Nil)
  }

  private def isLocallyDefined(sym: Symbol)(using Context) =
    sym.topLevelClass.isLinkedWith(pickler.rootCls)

  def pickleConstant(c: Constant)(using Context): Unit = c.tag match {
    case UnitTag =>
      writeByte(UNITconst)
    case BooleanTag =>
      writeByte(if (c.booleanValue) TRUEconst else FALSEconst)
    case ByteTag =>
      writeByte(BYTEconst)
      writeInt(c.byteValue)
    case ShortTag =>
      writeByte(SHORTconst)
      writeInt(c.shortValue)
    case CharTag =>
      writeByte(CHARconst)
      writeNat(c.charValue)
    case IntTag =>
      writeByte(INTconst)
      writeInt(c.intValue)
    case LongTag =>
      writeByte(LONGconst)
      writeLongInt(c.longValue)
    case FloatTag =>
      writeByte(FLOATconst)
      writeInt(java.lang.Float.floatToRawIntBits(c.floatValue))
    case DoubleTag =>
      writeByte(DOUBLEconst)
      writeLongInt(java.lang.Double.doubleToRawLongBits(c.doubleValue))
    case StringTag =>
      writeByte(STRINGconst)
      pickleName(c.stringValue.toTermName)
    case NullTag =>
      writeByte(NULLconst)
    case ClazzTag =>
      writeByte(CLASSconst)
      pickleType(c.typeValue)
  }

  def pickleVariances(tp: Type)(using Context): Unit = tp match
    case tp: HKTypeLambda if tp.isDeclaredVarianceLambda =>
      for v <- tp.declaredVariances do
        writeByte(
          if v.is(Covariant) then COVARIANT
          else if v.is(Contravariant) then CONTRAVARIANT
          else STABLE)
    case _ =>

  def pickleType(tpe0: Type, richTypes: Boolean = false)(using Context): Unit = {
    val tpe = tpe0.stripTypeVar
    try {
      val prev: Addr | Null = pickledTypes.lookup(tpe)
      if (prev == null) {
        pickledTypes(tpe) = currentAddr
        pickleNewType(tpe, richTypes)
      }
      else {
        writeByte(SHAREDtype)
        writeRef(prev.uncheckedNN)
      }
    }
    catch {
      case ex: AssertionError =>
        println(i"error when pickling type $tpe")
        throw ex
    }
  }

  private def pickleNewType(tpe: Type, richTypes: Boolean)(using Context): Unit = tpe match {
    case AppliedType(tycon, args) =>
      if tycon.typeSymbol == defn.MatchCaseClass then
        writeByte(MATCHCASEtype)
        withLength { args.foreach(pickleType(_)) }
      else
        writeByte(APPLIEDtype)
        withLength { pickleType(tycon); args.foreach(pickleType(_)) }
    case ConstantType(value) =>
      pickleConstant(value)
    case tpe: NamedType =>
      val sym = tpe.symbol
      def pickleExternalRef(sym: Symbol) = {
        val isShadowedRef =
          sym.isClass && tpe.prefix.member(sym.name).symbol != sym
        if sym.is(Flags.Private) || isShadowedRef then
          writeByte(if (tpe.isType) TYPEREFin else TERMREFin)
          withLength {
            pickleNameAndSig(sym.name, sym.signature, sym.targetName)
            pickleType(tpe.prefix)
            pickleType(sym.owner.typeRef)
          }
        else if isJavaPickle && sym == defn.FromJavaObjectSymbol then
          pickleType(defn.ObjectType) // when unpickling Java TASTy, replace by <FromJavaObject>
        else
          writeByte(if (tpe.isType) TYPEREF else TERMREF)
          pickleNameAndSig(sym.name, tpe.signature, sym.targetName)
          pickleType(tpe.prefix)
      }
      if (sym.is(Flags.Package)) {
        writeByte(if (tpe.isType) TYPEREFpkg else TERMREFpkg)
        pickleName(sym.fullName)
      }
      else if (tpe.prefix == NoPrefix) {
        writeByte(if (tpe.isType) TYPEREFdirect else TERMREFdirect)
        if Config.checkLevelsOnConstraints && !symRefs.contains(sym) && !sym.isPatternBound && !sym.hasAnnotation(defn.QuotedRuntimePatterns_patternTypeAnnot) then
          report.error(em"pickling reference to as yet undefined $tpe with symbol ${sym}", sym.srcPos)
        pickleSymRef(sym)
      }
      else tpe.designator match {
        case name: Name =>
          writeByte(if (tpe.isType) TYPEREF else TERMREF)
          pickleName(name); pickleType(tpe.prefix)
        case sym: Symbol =>
          if (isLocallyDefined(sym)) {
            writeByte(if (tpe.isType) TYPEREFsymbol else TERMREFsymbol)
            pickleSymRef(sym); pickleType(tpe.prefix)
          }
          else pickleExternalRef(sym)
      }
    case tpe: ThisType =>
      if (tpe.cls.is(Flags.Package) && !tpe.cls.isEffectiveRoot) {
        writeByte(TERMREFpkg)
        pickleName(tpe.cls.fullName)
      }
      else {
        writeByte(THIS)
        pickleType(tpe.tref)
      }
    case tpe: SuperType =>
      writeByte(SUPERtype)
      withLength { pickleType(tpe.thistpe); pickleType(tpe.supertpe) }
    case tpe: RecThis =>
      writeByte(RECthis)
      val binderAddr: Addr | Null = pickledTypes.lookup(tpe.binder)
      assert(binderAddr != null, tpe.binder)
      writeRef(binderAddr.uncheckedNN)
    case tpe: SkolemType =>
      pickleType(tpe.info)
    case tpe: RefinedType =>
      writeByte(REFINEDtype)
      withLength {
        pickleName(tpe.refinedName)
        pickleType(tpe.parent)
        pickleType(tpe.refinedInfo, richTypes = true)
      }
    case tpe: RecType =>
      writeByte(RECtype)
      pickleType(tpe.parent)
    case tpe: TypeBounds =>
      writeByte(TYPEBOUNDS)
      withLength {
        pickleType(tpe.lo, richTypes)
        if !tpe.isInstanceOf[AliasingBounds] then
          pickleType(tpe.hi, richTypes)
        pickleVariances(tpe.hi)
      }
    case tpe: AnnotatedType =>
      writeByte(ANNOTATEDtype)
      withLength { pickleType(tpe.parent, richTypes); pickleTree(tpe.annot.tree) }
    case tpe: AndType =>
      writeByte(ANDtype)
      withLength { pickleType(tpe.tp1, richTypes); pickleType(tpe.tp2, richTypes) }
    case tpe: OrType =>
      writeByte(ORtype)
      withLength { pickleType(tpe.tp1, richTypes); pickleType(tpe.tp2, richTypes) }
    case tpe: FlexibleType =>
      writeByte(FLEXIBLEtype)
      withLength { pickleType(tpe.underlying, richTypes)  }
    case tpe: ExprType =>
      writeByte(BYNAMEtype)
      pickleType(tpe.underlying)
    case tpe: HKTypeLambda =>
      pickleMethodic(TYPELAMBDAtype, tpe, EmptyFlags)
    case tpe: MatchType =>
      writeByte(MATCHtype)
      withLength {
        pickleType(tpe.bound)
        pickleType(tpe.scrutinee)
        tpe.cases.foreach(pickleType(_))
      }
    case tpe: PolyType if richTypes =>
      pickleMethodic(POLYtype, tpe, EmptyFlags)
    case tpe: MethodType if richTypes =>
      var mods = EmptyFlags
      if tpe.isContextualMethod then mods |= Given
      else if tpe.isImplicitMethod then mods |= Implicit
      pickleMethodic(METHODtype, tpe, mods)
    case tpe: ParamRef =>
      assert(pickleParamRef(tpe), s"orphan parameter reference: $tpe")
    case tpe: LazyRef =>
      pickleType(tpe.ref)
  }

  def pickleMethodic(tag: Int, tpe: LambdaType, mods: FlagSet)(using Context): Unit = {
    writeByte(tag)
    withLength {
      pickleType(tpe.resultType, richTypes = true)
      tpe.paramNames.lazyZip(tpe.paramInfos).foreach { (name, tpe) =>
        pickleType(tpe); pickleName(name)
      }
      if (mods != EmptyFlags) pickleFlags(mods, tpe.isTermLambda)
    }
  }

  def pickleParamRef(tpe: ParamRef)(using Context): Boolean = {
    val binder: Addr | Null = pickledTypes.lookup(tpe.binder)
    val pickled = binder != null
    if (pickled) {
      writeByte(PARAMtype)
      withLength { writeRef(binder.uncheckedNN); writeNat(tpe.paramNum) }
    }
    pickled
  }

  def pickleTpt(tpt: Tree)(using Context): Unit =
    pickleTree(tpt)

  def pickleTreeUnlessEmpty(tree: Tree)(using Context): Unit = {
    if (!tree.isEmpty) pickleTree(tree)
  }

  def pickleElidedUnlessEmpty(tree: Tree, tp: Type)(using Context): Unit =
    if !tree.isEmpty then
      writeByte(ELIDED)
      pickleType(tp)

  def pickleDef(tag: Int, mdef: MemberDef, tpt: Tree, rhs: Tree = EmptyTree, pickleParams: => Unit = ())(using Context): Unit = {
    val sym = mdef.symbol

    assert(symRefs(sym) == NoAddr, sym)
    registerDef(sym)
    writeByte(tag)
    val addr = currentAddr
    try
      withLength {
        pickleName(sym.name)
        pickleParams
        tpt match {
          case _: Template | _: Hole => pickleTree(tpt)
          case _ if tpt.isType => pickleTpt(tpt)
        }
        if isOutlinePickle && sym.isTerm && isJavaPickle then
          // TODO: if we introduce outline typing for Scala definitions
          // then we will need to update the check here
          pickleElidedUnlessEmpty(rhs, tpt.tpe)
        else
          pickleTreeUnlessEmpty(rhs)
        pickleModifiers(sym, mdef)
      }
    catch
      case ex: Throwable =>
        if !ctx.settings.YnoDecodeStacktraces.value
          && handleRecursive.underlyingStackOverflowOrNull(ex) != null then
          throw StackSizeExceeded(mdef)
        else
          throw ex
    if sym.is(Method) && sym.owner.isClass then
      profile.recordMethodSize(sym, (currentAddr.index - addr.index) max 1, mdef.span)
    for docCtx <- ctx.docCtx do
      val comment = docCtx.docstrings.lookup(sym)
      if comment != null then
        docStrings(mdef) = comment
  }

  def pickleParam(tree: Tree)(using Context): Unit = {
    registerTreeAddr(tree)
    tree match {
      case tree: ValDef  => pickleDef(PARAM, tree, tree.tpt)
      case tree: TypeDef => pickleDef(TYPEPARAM, tree, tree.rhs)
    }
  }

  def pickleParams(trees: List[Tree])(using Context): Unit = {
    trees.foreach(preRegister)
    trees.foreach(pickleParam)
  }

  def pickleStats(stats: List[Tree])(using Context): Unit = {
    stats.foreach(preRegister)
    stats.foreach(stat => if (!stat.isEmpty) pickleTree(stat))
  }

  def pickleTree(tree: Tree)(using Context): Unit = {
    val addr = registerTreeAddr(tree)
    if (addr != currentAddr) {
      writeByte(SHAREDterm)
      writeRef(addr)
    }
    else
      try tree match {
        case Ident(name) =>
          tree.tpe match {
            case tp: TermRef if name != nme.WILDCARD =>
              // wildcards are pattern bound, need to be preserved as ids.
              pickleType(tp)
            case tp =>
              writeByte(if (tree.isType) IDENTtpt else IDENT)
              pickleName(name)
              pickleType(tp)
          }
        case This(qual) =>
          // This may be needed when pickling a `This` inside a capture set. See #19662 and #19859.
          // In this case, we pickle the tree as null.asInstanceOf[tree.tpe].
          // Since the pickled tree is not the same as the input, special handling is needed
          // in the tree printer when testing the pickler. See [[PlainPrinter#homogenize]].
          inline def pickleCapturedThis =
            pickleTree(Literal(Constant(null)).cast(tree.tpe).withSpan(tree.span))
          if (qual.isEmpty)
            if tree.tpe.isSingleton then pickleType(tree.tpe)
            else pickleCapturedThis
          else
            tree.tpe match
              case ThisType(tref) =>
                writeByte(QUALTHIS)
                pickleTree(qual.withType(tref))
              case _ => pickleCapturedThis
        case Select(qual, name) =>
          name match {
            case OuterSelectName(_, levels) =>
              writeByte(SELECTouter)
              withLength {
                writeNat(levels)
                pickleTree(qual)
                val SkolemType(tp) = tree.tpe: @unchecked
                pickleType(tp)
              }
            case _ =>
              val sig = tree.tpe.signature
              var ename = tree.symbol.targetName
              val selectFromQualifier =
                name.isTypeName
                || qual.isInstanceOf[Hole] // holes have no symbol
                || sig == Signature.NotAMethod // no overload resolution necessary
                || !tree.denot.symbol.exists // polymorphic function type
                || tree.denot.asSingleDenotation.isRefinedMethod // refined methods have no defining class symbol
              if selectFromQualifier then
                writeByte(if name.isTypeName then SELECTtpt else SELECT)
                pickleNameAndSig(name, sig, ename)
                pickleTree(qual)
              else // select from owner
                writeByte(SELECTin)
                withLength {
                  pickleNameAndSig(name, tree.symbol.signature, ename)
                  pickleTree(qual)
                  pickleType(tree.symbol.owner.typeRef)
                }
          }
        case Apply(fun, args) =>
          if (fun.symbol eq defn.throwMethod) {
            writeByte(THROW)
            pickleTree(args.head)
          }
          else if fun.symbol.originalSignaturePolymorphic.exists then
            writeByte(APPLYsigpoly)
            withLength {
              pickleTree(fun)
              pickleType(fun.tpe.widenTermRefExpr, richTypes = true) // this widens to a MethodType, so need richTypes
              args.foreach(pickleTree)
            }
          else {
            writeByte(APPLY)
            withLength {
              pickleTree(fun)
              args.foreach(pickleTree)
            }
          }
        case TypeApply(fun, args) =>
          writeByte(TYPEAPPLY)
          withLength {
            pickleTree(fun)
            args.foreach(pickleTpt)
          }
        case Literal(const1) =>
          pickleConstant {
            tree.tpe match {
              case ConstantType(const2) => const2
              case _ => const1
            }
          }
        case Super(qual, mix) =>
          writeByte(SUPER)
          withLength {
            pickleTree(qual);
            if (!mix.isEmpty) {
              // mixinType being a TypeRef when mix is non-empty is enforced by TreeChecker#checkSuper
              val SuperType(_, mixinType: TypeRef) = tree.tpe: @unchecked
              pickleTree(mix.withType(mixinType))
            }
          }
        case New(tpt) =>
          writeByte(NEW)
          pickleTpt(tpt)
        case Typed(expr, tpt) =>
          writeByte(TYPED)
          withLength { pickleTree(expr); pickleTpt(tpt) }
        case NamedArg(name, arg) =>
          writeByte(NAMEDARG)
          pickleName(name)
          pickleTree(arg)
        case Assign(lhs, rhs) =>
          writeByte(ASSIGN)
          withLength { pickleTree(lhs); pickleTree(rhs) }
        case Block(stats, expr) =>
          writeByte(BLOCK)
          stats.foreach(preRegister)
          withLength { pickleTree(expr); stats.foreach(pickleTree) }
        case tree @ If(cond, thenp, elsep) =>
          writeByte(IF)
          withLength {
            if (tree.isInline) writeByte(INLINE)
            pickleTree(cond)
            pickleTree(thenp)
            pickleTree(elsep)
          }
        case Closure(env, meth, tpt) =>
          writeByte(LAMBDA)
          assert(env.isEmpty)
          withLength {
            pickleTree(meth)
            if (tpt.tpe.exists) pickleTpt(tpt)
          }
        case tree @ Match(selector, cases) =>
          writeByte(MATCH)
          withLength {
            if (tree.isInline)
              if (selector.isEmpty) writeByte(IMPLICIT)
              else { writeByte(INLINE); pickleTree(selector) }
            else pickleTree(selector)
            tree.cases.foreach(pickleTree)
          }
        case CaseDef(pat, guard, rhs) =>
          writeByte(CASEDEF)
          withLength { pickleTree(pat); pickleTree(rhs); pickleTreeUnlessEmpty(guard) }
        case Return(expr, from) =>
          writeByte(RETURN)
          withLength { pickleSymRef(from.symbol); pickleTreeUnlessEmpty(expr) }
        case WhileDo(cond, body) =>
          writeByte(WHILE)
          withLength { pickleTree(cond); pickleTree(body) }
        case Try(block, cases, finalizer) =>
          writeByte(TRY)
          withLength { pickleTree(block); cases.foreach(pickleTree); pickleTreeUnlessEmpty(finalizer) }
        case SeqLiteral(elems, elemtpt) =>
          writeByte(REPEATED)
          withLength { pickleTree(elemtpt); elems.foreach(pickleTree) }
        case tree @ Inlined(call, bindings, expansion) =>
          writeByte(INLINED)
          bindings.foreach(preRegister)
          withLength {
            pickleTree(expansion)
            if (!tree.inlinedFromOuterScope) pickleTree(call)
            bindings.foreach { b =>
              assert(b.isInstanceOf[DefDef] || b.isInstanceOf[ValDef])
              pickleTree(b)
            }
          }
        case Bind(name, body) =>
          val sym = tree.symbol
          registerDef(sym)
          writeByte(BIND)
          withLength {
            pickleName(name)
            pickleType(sym.info)
            pickleTree(body)
            pickleFlags(sym.flags &~ Case, sym.isTerm)
          }
        case Alternative(alts) =>
          writeByte(ALTERNATIVE)
          withLength { alts.foreach(pickleTree) }
        case UnApply(fun, implicits, patterns) =>
          writeByte(UNAPPLY)
          withLength {
            pickleTree(fun)
            for (implicitArg <- implicits) {
              writeByte(IMPLICITarg)
              pickleTree(implicitArg)
            }
            pickleType(tree.tpe)
            patterns.foreach(pickleTree)
          }
        case tree: ValDef =>
          pickleDef(VALDEF, tree, tree.tpt, tree.rhs)
        case tree: DefDef =>
          def pickleParamss(paramss: List[ParamClause]): Unit = paramss match
            case Nil =>
            case Nil :: rest =>
              writeByte(EMPTYCLAUSE)
              pickleParamss(rest)
            case (params @ (param1 :: _)) :: rest =>
              pickleParams(params)
              rest match
                case (param2 :: _) :: _
                if param1.isInstanceOf[untpd.TypeDef] == param2.isInstanceOf[untpd.TypeDef] =>
                  writeByte(SPLITCLAUSE)
                case _ =>
              pickleParamss(rest)
          pickleDef(DEFDEF, tree, tree.tpt, tree.rhs, pickleParamss(tree.paramss))
        case tree: TypeDef =>
          pickleDef(TYPEDEF, tree, tree.rhs)
        case tree: Template =>
          registerDef(tree.symbol)
          writeByte(TEMPLATE)
          val (params, rest) = decomposeTemplateBody(tree.body)
          withLength {
            pickleParams(params)
            tree.parents.foreach(pickleTree)
            val cinfo @ ClassInfo(_, _, _, _, selfInfo) = tree.symbol.owner.info: @unchecked
            if (!tree.self.isEmpty) {
              writeByte(SELFDEF)
              pickleName(tree.self.name)

              if (!tree.self.tpt.isEmpty) pickleTree(tree.self.tpt)
              else {
                if (!tree.self.isEmpty) registerTreeAddr(tree.self)
                pickleType {
                  selfInfo match {
                    case sym: Symbol => sym.info
                    case tp: Type => tp
                  }
                }
              }
            }
            if isJavaPickle then
              val rest0 = rest.dropWhile:
                case stat: ValOrDefDef => stat.symbol.is(Flags.Invisible)
                case _ => false
              if tree.constr.symbol.is(Flags.Invisible) then
                writeByte(SPLITCLAUSE)
                pickleStats(rest0)
              else
                pickleStats(tree.constr :: rest0)
            else
              pickleStats(tree.constr :: rest)
          }
        case Import(expr, selectors) =>
          writeByte(IMPORT)
          withLength {
            pickleTree(expr)
            pickleSelectors(selectors)
          }
        case Export(expr, selectors) =>
          writeByte(EXPORT)
          withLength {
            pickleTree(expr)
            pickleSelectors(selectors)
          }
        case PackageDef(pid, stats) =>
          writeByte(PACKAGE)
          withLength { pickleType(pid.tpe); pickleStats(stats) }
        case tree: TypeTree =>
          pickleType(tree.tpe)
        case SingletonTypeTree(ref) =>
          writeByte(SINGLETONtpt)
          pickleTree(ref)
        case RefinedTypeTree(parent, refinements) =>
          if (refinements.isEmpty) pickleTree(parent)
          else {
            val refineCls = refinements.head.symbol.owner.asClass
            registerDef(refineCls)
            pickledTypes(refineCls.typeRef) = currentAddr
            writeByte(REFINEDtpt)
            refinements.foreach(preRegister)
            withLength { pickleTree(parent); refinements.foreach(pickleTree) }
          }
        case AppliedTypeTree(tycon, args) =>
          writeByte(APPLIEDtpt)
          withLength { pickleTree(tycon); args.foreach(pickleTree) }
        case MatchTypeTree(bound, selector, cases) =>
          writeByte(MATCHtpt)
          withLength {
            if (!bound.isEmpty) pickleTree(bound)
            pickleTree(selector)
            cases.foreach(pickleTree)
          }
        case ByNameTypeTree(tp) =>
          writeByte(BYNAMEtpt)
          pickleTree(tp)
        case Annotated(tree, annot) =>
          writeByte(ANNOTATEDtpt)
          withLength { pickleTree(tree); pickleTree(annot) }
        case LambdaTypeTree(tparams, body) =>
          writeByte(LAMBDAtpt)
          withLength { pickleParams(tparams); pickleTree(body) }
        case TypeBoundsTree(lo, hi, alias) =>
          writeByte(TYPEBOUNDStpt)
          withLength {
            pickleTree(lo);
            if alias.isEmpty then
              if hi ne lo then pickleTree(hi)
            else
              pickleTree(hi)
              pickleTree(alias)
          }
        case tree @ Quote(body, Nil) =>
          // TODO: Add QUOTE tag to TASTy
          assert(body.isTerm,
            """Quote with type should not be pickled.
              |Quote with type should only exists after staging phase at staging level 0.""".stripMargin)
          pickleTree(
            // scala.quoted.runtime.Expr.quoted[<tree.bodyType>](<body>)
            ref(defn.QuotedRuntime_exprQuote)
              .appliedToType(tree.bodyType)
              .appliedTo(body)
              .withSpan(tree.span)
          )
        case Splice(expr) =>
          pickleTree( // TODO: Add SPLICE tag to TASTy
            // scala.quoted.runtime.Expr.splice[<tree.tpe>](<expr>)
            ref(defn.QuotedRuntime_exprSplice)
              .appliedToType(tree.tpe)
              .appliedTo(expr)
              .withSpan(tree.span)
          )
        case tree: QuotePattern =>
          // TODO: Add QUOTEPATTERN tag to TASTy
          pickleTree(QuotePatterns.encode(tree))
        case Hole(_, idx, args, _) =>
          writeByte(HOLE)
          withLength {
            writeNat(idx)
            pickleType(tree.tpe, richTypes = true)
            args.foreach { arg =>
              arg.tpe match
                case _: TermRef if arg.isType => writeByte(EXPLICITtpt)
                case _ =>
              pickleTree(arg)
            }
          }
      }
      catch {
        case ex: TypeError =>
          report.error(ex.toMessage, tree.srcPos.focus)
        case ex: AssertionError =>
          println(i"error when pickling tree $tree")
          throw ex
        case ex: MatchError =>
          println(i"error when pickling tree $tree")
          throw ex
      }
  }

  def pickleSelectors(selectors: List[untpd.ImportSelector])(using Context): Unit =
    for sel <- selectors do
      pickleSelector(IMPORTED, sel.imported)
      sel.renamed match
        case to @ Ident(_) => pickleSelector(RENAMED, to)
        case _ =>
      sel.bound match
        case bound @ untpd.TypedSplice(tpt) =>
          registerTreeAddr(bound)
          writeByte(BOUNDED)
          pickleTree(tpt)
        case _ =>

  def pickleSelector(tag: Int, id: untpd.Ident)(using Context): Unit = {
    registerTreeAddr(id)
    writeByte(tag)
    pickleName(id.name)
  }

  def pickleModifiers(sym: Symbol, mdef: MemberDef)(using Context): Unit = {
    import Flags.*
    var flags = sym.flags
    val privateWithin = sym.privateWithin
    if (privateWithin.exists) {
      writeByte(if (flags.is(Protected)) PROTECTEDqualified else PRIVATEqualified)
      pickleType(privateWithin.typeRef)
      flags = flags &~ Protected
    }
    if (flags.is(ParamAccessor) && sym.isTerm && !sym.isSetter)
      flags = flags &~ ParamAccessor // we only generate a tag for parameter setters
    pickleFlags(flags, sym.isTerm)
    val annots = sym.annotations.foreach(pickleAnnotation(sym, mdef, _))
  }

  def pickleFlags(flags: FlagSet, isTerm: Boolean)(using Context): Unit = {
    import Flags.*
    def writeModTag(tag: Int) = {
      assert(isModifierTag(tag))
      writeByte(tag)
    }
    assert(!flags.is(Scala2x))
    if (flags.is(Private)) writeModTag(PRIVATE)
    if (flags.is(Protected)) writeModTag(PROTECTED)
    if (flags.is(Final, butNot = Module)) writeModTag(FINAL)
    if (flags.is(Case)) writeModTag(CASE)
    if (flags.is(Override)) writeModTag(OVERRIDE)
    if (flags.is(Inline)) writeModTag(INLINE)
    if (flags.is(InlineProxy)) writeModTag(INLINEPROXY)
    if (flags.is(Macro)) writeModTag(MACRO)
    if (flags.is(JavaStatic)) writeModTag(STATIC)
    if (flags.is(Module)) writeModTag(OBJECT)
    if (flags.is(Enum)) writeModTag(ENUM)
    if (flags.is(Local)) writeModTag(LOCAL)
    if (flags.is(Synthetic)) writeModTag(SYNTHETIC)
    if (flags.is(Artifact)) writeModTag(ARTIFACT)
    if flags.is(Transparent) then writeModTag(TRANSPARENT)
    if flags.is(Infix) then writeModTag(INFIX)
    if flags.is(Invisible) then writeModTag(INVISIBLE)
    if (flags.is(Erased)) writeModTag(ERASED)
    if (flags.is(Exported)) writeModTag(EXPORTED)
    if (flags.is(Given)) writeModTag(GIVEN)
    if (flags.is(Implicit)) writeModTag(IMPLICIT)
    if (isTerm) {
      if (flags.is(Lazy, butNot = Module)) writeModTag(LAZY)
      if (flags.is(AbsOverride)) { writeModTag(ABSTRACT); writeModTag(OVERRIDE) }
      if (flags.is(Mutable)) writeModTag(MUTABLE)
      if (flags.is(Accessor)) writeModTag(FIELDaccessor)
      if (flags.is(CaseAccessor)) writeModTag(CASEaccessor)
      if (flags.is(HasDefault)) writeModTag(HASDEFAULT)
      if flags.isAllOf(StableMethod) then writeModTag(STABLE) // other StableRealizable flag occurrences are either implied or can be recomputed
      if (flags.is(Extension)) writeModTag(EXTENSION)
      if (flags.is(ParamAccessor)) writeModTag(PARAMsetter)
      if (flags.is(SuperParamAlias)) writeModTag(PARAMalias)
      assert(!(flags.is(Label)))
    }
    else {
      if (flags.is(Sealed)) writeModTag(SEALED)
      if (flags.is(Abstract)) writeModTag(ABSTRACT)
      if (flags.is(Trait)) writeModTag(TRAIT)
      if (flags.is(Covariant)) writeModTag(COVARIANT)
      if (flags.is(Contravariant)) writeModTag(CONTRAVARIANT)
      if (flags.is(Opaque)) writeModTag(OPAQUE)
      if (flags.is(Open)) writeModTag(OPEN)
    }
  }

  private def isUnpicklable(owner: Symbol, ann: Annotation)(using Context) = ann match {
    case Annotation.Child(sym) => sym.isInaccessibleChildOf(owner)
      // If child annotation refers to a local class or enum value under
      // a different toplevel class, it is impossible to pickle a reference to it.
      // Such annotations will be reconstituted when unpickling the child class.
      // See tests/pickling/i3149.scala
    case _ =>
      ann.symbol == defn.BodyAnnot // inline bodies are reconstituted automatically when unpickling
  }

  def pickleAnnotation(owner: Symbol, mdef: MemberDef, ann: Annotation)(using Context): Unit =
    if !isUnpicklable(owner, ann) then
      writeByte(ANNOTATION)
      withLength { pickleType(ann.symbol.typeRef); pickleTree(ann.tree) }
      var treeBuf = annotTrees.lookup(mdef)
      if treeBuf == null then
        treeBuf = new mutable.ListBuffer[Tree]
        annotTrees(mdef) = treeBuf
      treeBuf += ann.tree

// ---- main entry points ---------------------------------------

  def pickle(trees: List[Tree])(using Context): Unit = {
    profile = Profile.current
    for tree <- trees do
      try
        if !tree.isEmpty then pickleTree(tree)
      catch case ex: StackSizeExceeded =>
        report.error(
          em"""Recursion limit exceeded while pickling ${ex.mdef}
              |in ${ex.mdef.symbol.showLocated}.
              |You could try to increase the stacksize using the -Xss JVM option.
              |For the unprocessed stack trace, compile with -Yno-decode-stacktraces.""",
          ex.mdef.srcPos)

    def missing = forwardSymRefs.keysIterator
      .map(sym => i"${sym.showLocated} (line ${sym.srcPos.line}) #${sym.id}")
      .toList
    assert(forwardSymRefs.isEmpty, i"unresolved symbols: $missing%, % when pickling ${ctx.source}")
  }

  def compactify(scratch: ScratchData = new ScratchData): Unit = {
    buf.compactify(scratch)

    def updateMapWithDeltas(mp: MutableSymbolMap[Addr]) =
      val keys = new Array[Symbol](mp.size)
      val it = mp.keysIterator
      var i = 0
      while i < keys.length do
        keys(i) = it.next
        i += 1
      assert(!it.hasNext)
      i = 0
      while i < keys.length do
        val key = keys(i)
        mp(key) = adjusted(mp(key), scratch)
        i += 1

    updateMapWithDeltas(symRefs)
  }
}
