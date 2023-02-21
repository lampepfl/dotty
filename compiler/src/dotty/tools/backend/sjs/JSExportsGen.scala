package dotty.tools.backend.sjs

import scala.language.unsafeNulls

import scala.annotation.tailrec
import scala.collection.mutable

import dotty.tools.dotc.core._

import Contexts._
import Decorators._
import Denotations._
import Flags._
import Names._
import NameKinds.DefaultGetterName
import NameOps._
import Phases._
import Symbols._
import Types._
import TypeErasure.ErasedValueType

import dotty.tools.dotc.util.{SourcePosition, SrcPos}
import dotty.tools.dotc.report

import org.scalajs.ir.{Position, Names => jsNames, Trees => js, Types => jstpe}
import org.scalajs.ir.Names.DefaultModuleID
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.Trees.OptimizerHints

import dotty.tools.dotc.transform.sjs.JSExportUtils._
import dotty.tools.dotc.transform.sjs.JSSymUtils._

import JSEncoding._

final class JSExportsGen(jsCodeGen: JSCodeGen)(using Context) {
  import jsCodeGen._
  import positionConversions._

  /** Info for a non-member export. */
  sealed trait ExportInfo {
    val pos: SourcePosition
  }

  final case class TopLevelExportInfo(moduleID: String, jsName: String)(val pos: SourcePosition) extends ExportInfo
  final case class StaticExportInfo(jsName: String)(val pos: SourcePosition) extends ExportInfo

  private sealed trait ExportKind

  private object ExportKind {
    case object Module extends ExportKind
    case object JSClass extends ExportKind
    case object Constructor extends ExportKind
    case object Method extends ExportKind
    case object Property extends ExportKind
    case object Field extends ExportKind

    def apply(sym: Symbol): ExportKind = {
      if (sym.is(Flags.Module) && sym.isStatic) Module
      else if (sym.isClass) JSClass
      else if (sym.isConstructor) Constructor
      else if (!sym.is(Flags.Method)) Field
      else if (sym.isJSProperty) Property
      else Method
    }
  }

  private def topLevelExportsOf(sym: Symbol): List[TopLevelExportInfo] = {
    def isScalaClass(sym: Symbol): Boolean =
      sym.isClass && !sym.isOneOf(Module | Trait) && !sym.isJSType

    if (isScalaClass(sym)) {
      // Scala classes are never exported; their constructors are
      Nil
    } else if (sym.is(Accessor) || sym.is(Module, butNot = ModuleClass)) {
      /* - Accessors receive the `@JSExportTopLevel` annotation of their associated field,
       *   but only the field is really exported.
       * - Module values are not exported; their module class takes care of the export.
       */
      Nil
    } else {
      val symForAnnot =
        if (sym.isConstructor && isScalaClass(sym.owner)) sym.owner
        else sym

      symForAnnot.annotations.collect {
        case annot if annot.symbol == jsdefn.JSExportTopLevelAnnot =>
          val jsName = annot.argumentConstantString(0).get
          val moduleID = annot.argumentConstantString(1).getOrElse(DefaultModuleID)
          TopLevelExportInfo(moduleID, jsName)(annot.tree.sourcePos)
      }
    }
  }

  private def staticExportsOf(sym: Symbol): List[StaticExportInfo] = {
    if (sym.is(Accessor)) {
      Nil
    } else {
      sym.annotations.collect {
        case annot if annot.symbol == jsdefn.JSExportStaticAnnot =>
          val jsName = annot.argumentConstantString(0).getOrElse {
            sym.defaultJSName
          }
          StaticExportInfo(jsName)(annot.tree.sourcePos)
      }
    }
  }

  private def checkSameKind(tups: List[(ExportInfo, Symbol)]): Option[ExportKind] = {
    assert(tups.nonEmpty, "must have at least one export")

    val firstSym = tups.head._2
    val overallKind = ExportKind(firstSym)
    var bad = false

    for ((info, sym) <- tups.tail) {
      val kind = ExportKind(sym)

      if (kind != overallKind) {
        bad = true
        report.error(
            em"export overload conflicts with export of $firstSym: they are of different types (${kind.tryToShow} / ${overallKind.tryToShow})",
            info.pos)
      }
    }

    if (bad) None
    else Some(overallKind)
  }

  private def checkSingleField(tups: List[(ExportInfo, Symbol)]): Symbol = {
    assert(tups.nonEmpty, "must have at least one export")

    val firstSym = tups.head._2

    for ((info, _) <- tups.tail) {
      report.error(
          em"export overload conflicts with export of $firstSym: a field may not share its exported name with another export",
          info.pos)
    }

    firstSym
  }

  def genTopLevelExports(classSym: ClassSymbol): List[js.TopLevelExportDef] = {
    val exports = for {
      sym <- classSym :: classSym.info.decls.toList
      info <- topLevelExportsOf(sym)
    } yield {
      (info, sym)
    }

    (for {
      (info, tups) <- exports.groupBy(_._1)
      kind <- checkSameKind(tups)
    } yield {
      import ExportKind._

      implicit val pos = info.pos

      kind match {
        case Module =>
          js.TopLevelModuleExportDef(info.moduleID, info.jsName)

        case JSClass =>
          assert(classSym.isNonNativeJSClass, "found export on non-JS class")
          js.TopLevelJSClassExportDef(info.moduleID, info.jsName)

        case Constructor | Method =>
          val exported = tups.map(_._2)

          val methodDef = withNewLocalNameScope {
            genExportMethod(exported, JSName.Literal(info.jsName), static = true)
          }

          js.TopLevelMethodExportDef(info.moduleID, methodDef)

        case Property =>
          ctx.implode("found top-level exported property")

        case Field =>
          val sym = checkSingleField(tups)
          js.TopLevelFieldExportDef(info.moduleID, info.jsName, encodeFieldSym(sym))
      }
    }).toList
  }

  def genStaticExports(classSym: Symbol): List[js.MemberDef] = {
    val exports = for {
      sym <- classSym.info.decls.toList
      info <- staticExportsOf(sym)
    } yield {
      (info, sym)
    }

    (for {
      (info, tups) <- exports.groupBy(_._1)
      kind <- checkSameKind(tups)
    } yield {
      def alts = tups.map(_._2)

      implicit val pos = info.pos

      import ExportKind._

      kind match {
        case Method =>
          genMemberExportOrDispatcher(JSName.Literal(info.jsName), isProp = false, alts, static = true)

        case Property =>
          genMemberExportOrDispatcher(JSName.Literal(info.jsName), isProp = true, alts, static = true)

        case Field =>
          val sym = checkSingleField(tups)

          // static fields must always be mutable
          val flags = js.MemberFlags.empty
            .withNamespace(js.MemberNamespace.PublicStatic)
            .withMutable(true)
          val name = js.StringLiteral(info.jsName)
          val irTpe = genExposedFieldIRType(sym)
          js.JSFieldDef(flags, name, irTpe)

        case kind =>
          ctx.implode(s"unexpected static export kind: $kind")
      }
    }).toList
  }

  /** Generates exported methods and properties for a class.
   *
   *  @param classSym symbol of the class we export for
   */
  def genMemberExports(classSym: ClassSymbol): List[js.MemberDef] = {
    val classInfo = classSym.info
    val allExports = classInfo.memberDenots(takeAllFilter, { (name, buf) =>
      if (isExportName(name))
        buf ++= classInfo.member(name).alternatives
    })

    val newlyDeclaredExports = if (classSym.superClass == NoSymbol) {
      allExports
    } else {
      allExports.filterNot { denot =>
        classSym.superClass.info.member(denot.name).hasAltWith(_.info =:= denot.info)
      }
    }

    val newlyDeclaredExportNames = newlyDeclaredExports.map(_.name.toTermName).toList.distinct

    newlyDeclaredExportNames.map(genMemberExport(classSym, _))
  }

  private def genMemberExport(classSym: ClassSymbol, name: TermName): js.MemberDef = {
    /* This used to be `.member(name)`, but it caused #3538, since we were
     * sometimes selecting mixin forwarders, whose type history does not go
     * far enough back in time to see varargs. We now explicitly exclude
     * mixed-in members in addition to bridge methods (the latter are always
     * excluded by `.member(name)`).
     */
    val alts = classSym
      .findMemberNoShadowingBasedOnFlags(name, classSym.appliedRef, required = Method, excluded = Bridge | MixedIn)
      .alternatives

    assert(!alts.isEmpty,
        em"""Ended up with no alternatives for ${classSym.fullName}::$name.
            |Original set was ${alts} with types ${alts.map(_.info)}""")

    val (jsName, isProp) = exportNameInfo(name)

    // Check if we have a conflicting export of the other kind
    val conflicting = classSym.info.member(makeExportName(jsName, !isProp))

    if (conflicting.exists) {
      val kind = if (isProp) "property" else "method"
      val conflictingMember = conflicting.alternatives.head.symbol.fullName
      val errorPos: SrcPos = alts.map(_.symbol).filter(_.owner == classSym) match {
        case Nil         => classSym
        case altsInClass => altsInClass.minBy(_.span.point)
      }
      report.error(em"Exported $kind $jsName conflicts with $conflictingMember", errorPos)
    }

    genMemberExportOrDispatcher(JSName.Literal(jsName), isProp, alts.map(_.symbol), static = false)
  }

  def genJSClassDispatchers(classSym: Symbol, dispatchMethodsNames: List[JSName]): List[js.MemberDef] = {
    dispatchMethodsNames.map(genJSClassDispatcher(classSym, _))
  }

  private def genJSClassDispatcher(classSym: Symbol, name: JSName): js.MemberDef = {
    val alts = classSym.info.membersBasedOnFlags(required = Method, excluded = Bridge)
      .map(_.symbol)
      .filter { sym =>
        /* scala-js#3939: Object is not a "real" superclass of JS types.
         * as such, its methods do not participate in overload resolution.
         * An exception is toString, which is handled specially in genExportMethod.
         */
        sym.owner != defn.ObjectClass && sym.jsName == name
      }
      .toList

    assert(!alts.isEmpty, s"Ended up with no alternatives for ${classSym.fullName}::$name.")

    val (propSyms, methodSyms) = alts.partition(_.isJSProperty)
    val isProp = propSyms.nonEmpty

    if (isProp && methodSyms.nonEmpty) {
      val firstAlt = alts.head
      report.error(
          em"Conflicting properties and methods for ${classSym.fullName}::$name.",
          firstAlt.srcPos)
      implicit val pos = firstAlt.span
      js.JSPropertyDef(js.MemberFlags.empty, genExpr(name)(firstAlt.sourcePos), None, None)
    } else {
      genMemberExportOrDispatcher(name, isProp, alts, static = false)
    }
  }

  private def genMemberExportOrDispatcher(jsName: JSName, isProp: Boolean,
      alts: List[Symbol], static: Boolean): js.MemberDef = {
    withNewLocalNameScope {
      if (isProp)
        genExportProperty(alts, jsName, static)
      else
        genExportMethod(alts, jsName, static)
    }
  }

  private def genExportProperty(alts: List[Symbol], jsName: JSName, static: Boolean): js.JSPropertyDef = {
    assert(!alts.isEmpty, s"genExportProperty with empty alternatives for $jsName")

    implicit val pos: Position = alts.head.span

    val namespace =
      if (static) js.MemberNamespace.PublicStatic
      else js.MemberNamespace.Public
    val flags = js.MemberFlags.empty.withNamespace(namespace)

    /* Separate getters and setters. Since we only have getters and setters, we
     * simply test the param list size, which is faster than using the full isJSGetter.
     */
    val (getter, setters) = alts.partition(_.info.paramInfoss.head.isEmpty)

    // We can have at most one getter
    if (getter.sizeIs > 1)
      reportCannotDisambiguateError(jsName, alts)

    val getterBody = getter.headOption.map { getterSym =>
      genApplyForSingleExported(new FormalArgsRegistry(0, false), new ExportedSymbol(getterSym, static), static)
    }

    val setterArgAndBody = {
      if (setters.isEmpty) {
        None
      } else {
        val formalArgsRegistry = new FormalArgsRegistry(1, false)
        val (List(arg), None) = formalArgsRegistry.genFormalArgs(): @unchecked
        val body = genOverloadDispatchSameArgc(jsName, formalArgsRegistry,
            setters.map(new ExportedSymbol(_, static)), jstpe.AnyType, None)
        Some((arg, body))
      }
    }

    js.JSPropertyDef(flags, genExpr(jsName)(alts.head.sourcePos), getterBody, setterArgAndBody)
  }

  private def genExportMethod(alts0: List[Symbol], jsName: JSName, static: Boolean)(using Context): js.JSMethodDef = {
    assert(alts0.nonEmpty, "need at least one alternative to generate exporter method")

    implicit val pos: SourcePosition = alts0.head.sourcePos

    val namespace =
      if (static) js.MemberNamespace.PublicStatic
      else js.MemberNamespace.Public
    val flags = js.MemberFlags.empty.withNamespace(namespace)

    // toString() is always exported. We might need to add it here to get correct overloading.
    val alts = jsName match {
      case JSName.Literal("toString") if alts0.forall(_.info.paramInfoss.exists(_.nonEmpty)) =>
        defn.Any_toString :: alts0
      case _ =>
        alts0
    }

    val overloads = alts.map(new ExportedSymbol(_, static))

    val (formalArgs, restParam, body) =
      genOverloadDispatch(jsName, overloads, jstpe.AnyType)

    js.JSMethodDef(flags, genExpr(jsName), formalArgs, restParam, body)(
        OptimizerHints.empty, None)
  }

  def genOverloadDispatch(jsName: JSName, alts: List[Exported], tpe: jstpe.Type)(
      using pos: SourcePosition): (List[js.ParamDef], Option[js.ParamDef], js.Tree) = {

    // Create the formal args registry
    val hasVarArg = alts.exists(_.hasRepeatedParam)
    val minArgc = alts.map(_.minArgc).min
    val maxNonRepeatedArgc = alts.map(_.maxNonRepeatedArgc).max
    val needsRestParam = maxNonRepeatedArgc != minArgc || hasVarArg
    val formalArgsRegistry = new FormalArgsRegistry(minArgc, needsRestParam)

    // Generate the list of formal parameters
    val (formalArgs, restParam) = formalArgsRegistry.genFormalArgs()

    /* Generate the body
     * We have a fast-path for methods that are not overloaded. In addition to
     * being a fast path, it does a better job than `genExportMethodMultiAlts`
     * when the only alternative has default parameters, because it avoids a
     * spurious dispatch.
     * In scalac, the spurious dispatch was avoided by a more elaborate case
     * generation in `genExportMethod`, which was very convoluted and was not
     * ported to dotc.
     */
    val body =
      if (alts.tail.isEmpty) alts.head.genBody(formalArgsRegistry)
      else genExportMethodMultiAlts(formalArgsRegistry, maxNonRepeatedArgc, alts, tpe, jsName)

    (formalArgs, restParam, body)
  }

  private def genExportMethodMultiAlts(formalArgsRegistry: FormalArgsRegistry,
      maxNonRepeatedArgc: Int, alts: List[Exported], tpe: jstpe.Type, jsName: JSName)(
      implicit pos: SourcePosition): js.Tree = {

    // Generate tuples (argc, method)
    val methodArgCounts = for {
      alt <- alts
      argc <- alt.minArgc to (if (alt.hasRepeatedParam) maxNonRepeatedArgc else alt.maxNonRepeatedArgc)
    } yield {
      (argc, alt)
    }

    // Create a list of (argCount -> methods), sorted by argCount (methods may appear multiple times)
    val methodsByArgCount: List[(Int, List[Exported])] =
      methodArgCounts.groupMap(_._1)(_._2).toList.sortBy(_._1) // sort for determinism

    val altsWithVarArgs = alts.filter(_.hasRepeatedParam)

    // Generate a case block for each (argCount, methods) tuple
    // TODO? We could optimize this a bit by putting together all the `argCount`s that have the same methods
    // (Scala.js for scalac does that, but the code is very convoluted and it's not clear that it is worth it).
    val cases = for {
      (argc, methods) <- methodsByArgCount
      if methods != altsWithVarArgs // exclude default case we're generating anyways for varargs
    } yield {
      // body of case to disambiguates methods with current count
      val caseBody = genOverloadDispatchSameArgc(jsName, formalArgsRegistry, methods, tpe, Some(argc))
      List(js.IntLiteral(argc - formalArgsRegistry.minArgc)) -> caseBody
    }

    def defaultCase = {
      if (altsWithVarArgs.isEmpty)
        genThrowTypeError()
      else
        genOverloadDispatchSameArgc(jsName, formalArgsRegistry, altsWithVarArgs, tpe, None)
    }

    val body = {
      if (cases.isEmpty) {
        defaultCase
      } else if (cases.tail.isEmpty && altsWithVarArgs.isEmpty) {
        cases.head._2
      } else {
        val restArgRef = formalArgsRegistry.genRestArgRef()
        js.Match(
            js.AsInstanceOf(js.JSSelect(restArgRef, js.StringLiteral("length")), jstpe.IntType),
            cases,
            defaultCase)(
            tpe)
      }
    }

    body
  }

  /** Resolves method calls to [[alts]] while assuming they have the same parameter count.
   *
   *  @param jsName
   *    The JS name of the method, for error reporting
   *  @param formalArgsRegistry
   *    The registry of all the formal arguments
   *  @param alts
   *    Alternative methods
   *  @param tpe
   *    Result type
   *  @param maxArgc
   *    Maximum number of arguments to use for disambiguation
   */
  private def genOverloadDispatchSameArgc(jsName: JSName, formalArgsRegistry: FormalArgsRegistry,
      alts: List[Exported], tpe: jstpe.Type, maxArgc: Option[Int]): js.Tree = {
    genOverloadDispatchSameArgcRec(jsName, formalArgsRegistry, alts, tpe, paramIndex = 0, maxArgc)
  }

  /** Resolves method calls to [[alts]] while assuming they have the same parameter count.
   *
   *  @param jsName
   *    The JS name of the method, for error reporting
   *  @param formalArgsRegistry
   *    The registry of all the formal arguments
   *  @param alts
   *    Alternative methods
   *  @param tpe
   *    Result type
   *  @param paramIndex
   *    Index where to start disambiguation (starts at 0, increases through recursion)
   *  @param maxArgc
   *    Maximum number of arguments to use for disambiguation
   */
  private def genOverloadDispatchSameArgcRec(jsName: JSName, formalArgsRegistry: FormalArgsRegistry,
      alts: List[Exported], tpe: jstpe.Type, paramIndex: Int, maxArgc: Option[Int]): js.Tree = {

    implicit val pos = alts.head.pos

    if (alts.sizeIs == 1) {
      alts.head.genBody(formalArgsRegistry)
    } else if (maxArgc.exists(_ <= paramIndex) || !alts.exists(_.params.size > paramIndex)) {
      // We reach here in three cases:
      // 1. The parameter list has been exhausted
      // 2. The optional argument count restriction has triggered
      // 3. We only have (more than once) repeated parameters left
      // Therefore, we should fail
      reportCannotDisambiguateError(jsName, alts.map(_.sym))
      js.Undefined()
    } else {
      val altsByTypeTest = groupByWithoutHashCode(alts) { exported =>
        typeTestForTpe(exported.exportArgTypeAt(paramIndex))
      }

      if (altsByTypeTest.size == 1) {
        // Testing this parameter is not doing any us good
        genOverloadDispatchSameArgcRec(jsName, formalArgsRegistry, alts, tpe, paramIndex + 1, maxArgc)
      } else {
        // Sort them so that, e.g., isInstanceOf[String] comes before isInstanceOf[Object]
        val sortedAltsByTypeTest = topoSortDistinctsWith(altsByTypeTest) { (lhs, rhs) =>
          (lhs._1, rhs._1) match {
            // NoTypeTest is always last
            case (_, NoTypeTest) => true
            case (NoTypeTest, _) => false

            case (PrimitiveTypeTest(_, rank1), PrimitiveTypeTest(_, rank2)) =>
              rank1 <= rank2

            case (InstanceOfTypeTest(t1), InstanceOfTypeTest(t2)) =>
              t1 <:< t2

            case (_: PrimitiveTypeTest, _: InstanceOfTypeTest) => true
            case (_: InstanceOfTypeTest, _: PrimitiveTypeTest) => false
          }
        }

        val defaultCase = genThrowTypeError()

        sortedAltsByTypeTest.foldRight[js.Tree](defaultCase) { (elem, elsep) =>
          val (typeTest, subAlts) = elem
          implicit val pos = subAlts.head.pos

          val paramRef = formalArgsRegistry.genArgRef(paramIndex)
          val genSubAlts = genOverloadDispatchSameArgcRec(jsName, formalArgsRegistry,
              subAlts, tpe, paramIndex + 1, maxArgc)

          def hasDefaultParam = subAlts.exists(_.hasDefaultAt(paramIndex))

          val optCond = typeTest match {
            case PrimitiveTypeTest(tpe, _) => Some(js.IsInstanceOf(paramRef, tpe))
            case InstanceOfTypeTest(tpe)   => Some(genIsInstanceOf(paramRef, tpe))
            case NoTypeTest                => None
          }

          optCond.fold[js.Tree] {
            genSubAlts // note: elsep is discarded, obviously
          } { cond =>
            val condOrUndef = if (!hasDefaultParam) cond else {
              js.If(cond, js.BooleanLiteral(true),
                  js.BinaryOp(js.BinaryOp.===, paramRef, js.Undefined()))(
                  jstpe.BooleanType)
            }
            js.If(condOrUndef, genSubAlts, elsep)(tpe)
          }
        }
      }
    }
  }

  private def reportCannotDisambiguateError(jsName: JSName, alts: List[Symbol]): Unit = {
    val currentClass = currentClassSym.get

    /* Find a position that is in the current class for decent error reporting.
     * If there are more than one, always use the "highest" one (i.e., the
     * one coming last in the source text) so that we reliably display the
     * same error in all compilers.
     */
    val validPositions = alts.collect {
      case alt if alt.owner == currentClass => alt.sourcePos
    }
    val pos: SourcePosition =
      if (validPositions.isEmpty) currentClass.sourcePos
      else validPositions.maxBy(_.point)

    val kind =
      if (alts.head.isJSGetter) "getter"
      else if (alts.head.isJSSetter) "setter"
      else "method"

    val fullKind =
      if (currentClass.isJSType) kind
      else "exported " + kind

    val displayName = jsName.displayName
    val altsTypesInfo = alts.map(_.info.show).sorted.mkString("\n  ")

    report.error(
        em"Cannot disambiguate overloads for $fullKind $displayName with types\n  $altsTypesInfo",
        pos)
  }

  /** Generates a call to the method represented by the given `exported` while using the formalArguments
   *  and potentially the argument array.
   *
   *  Also inserts default parameters if required.
   */
  private def genApplyForSingleExported(formalArgsRegistry: FormalArgsRegistry,
      exported: Exported, static: Boolean): js.Tree = {
    if (currentClassSym.isJSType && exported.sym.owner != currentClassSym.get) {
      assert(!static, s"nonsensical JS super call in static export of ${exported.sym}")
      genApplyForSingleExportedJSSuperCall(formalArgsRegistry, exported)
    } else {
      genApplyForSingleExportedNonJSSuperCall(formalArgsRegistry, exported, static)
    }
  }

  private def genApplyForSingleExportedJSSuperCall(
      formalArgsRegistry: FormalArgsRegistry, exported: Exported): js.Tree = {
    implicit val pos = exported.pos

    val sym = exported.sym
    assert(!sym.isClassConstructor,
        s"Trying to genApplyForSingleExportedJSSuperCall for the constructor ${sym.fullName}")

    val allArgs = formalArgsRegistry.genAllArgsRefsForForwarder()

    val superClass = {
      val superClassSym = currentClassSym.asClass.superClass
      if (superClassSym.isNestedJSClass)
        js.VarRef(js.LocalIdent(JSSuperClassParamName))(jstpe.AnyType)
      else
        js.LoadJSConstructor(encodeClassName(superClassSym))
    }

    val receiver = js.This()(currentThisType)
    val nameTree = genExpr(sym.jsName)

    if (sym.isJSGetter) {
      assert(allArgs.isEmpty,
          s"getter symbol $sym does not have a getter signature")
      js.JSSuperSelect(superClass, receiver, nameTree)
    } else if (sym.isJSSetter) {
      assert(allArgs.size == 1 && allArgs.head.isInstanceOf[js.Tree],
          s"setter symbol $sym does not have a setter signature")
      js.Assign(js.JSSuperSelect(superClass, receiver, nameTree),
          allArgs.head.asInstanceOf[js.Tree])
    } else {
      js.JSSuperMethodCall(superClass, receiver, nameTree, allArgs)
    }
  }

  private def genApplyForSingleExportedNonJSSuperCall(
      formalArgsRegistry: FormalArgsRegistry, exported: Exported, static: Boolean): js.Tree = {

    implicit val pos = exported.pos

    val varDefs = new mutable.ListBuffer[js.VarDef]

    for ((param, i) <- exported.params.zipWithIndex) {
      val rhs = genScalaArg(exported, i, formalArgsRegistry, param, static, captures = Nil)(
          prevArgsCount => varDefs.take(prevArgsCount).toList.map(_.ref))

      varDefs += js.VarDef(freshLocalIdent("prep" + i), NoOriginalName, rhs.tpe, mutable = false, rhs)
    }

    val builtVarDefs = varDefs.result()

    val jsResult = genResult(exported, builtVarDefs.map(_.ref), static)

    js.Block(builtVarDefs :+ jsResult)
  }

  /** Generates a Scala argument from dispatched JavaScript arguments
   *  (unboxing and default parameter handling).
   */
  def genScalaArg(exported: Exported, paramIndex: Int, formalArgsRegistry: FormalArgsRegistry,
      param: JSParamInfo, static: Boolean, captures: List[js.Tree])(
      previousArgsValues: Int => List[js.Tree])(
      implicit pos: SourcePosition): js.Tree = {

    if (param.repeated) {
      genJSArrayToVarArgs(formalArgsRegistry.genVarargRef(paramIndex))
    } else {
      val jsArg = formalArgsRegistry.genArgRef(paramIndex)

      // Unboxed argument (if it is defined)
      val unboxedArg = unbox(jsArg, param.info)

      if (exported.hasDefaultAt(paramIndex)) {
        // If argument is undefined and there is a default getter, call it
        js.If(js.BinaryOp(js.BinaryOp.===, jsArg, js.Undefined()), {
          genCallDefaultGetter(exported.sym, paramIndex, static, captures)(previousArgsValues)
        }, {
          unboxedArg
        })(unboxedArg.tpe)
      } else {
        // Otherwise, it is always the unboxed argument
        unboxedArg
      }
    }
  }

  def genCallDefaultGetter(sym: Symbol, paramIndex: Int,
      static: Boolean, captures: List[js.Tree])(
      previousArgsValues: Int => List[js.Tree])(
      implicit pos: SourcePosition): js.Tree = {

    val targetSym = targetSymForDefaultGetter(sym)
    val defaultGetterDenot = this.defaultGetterDenot(targetSym, sym, paramIndex)

    assert(defaultGetterDenot.exists, s"need default getter for method ${sym.fullName}")
    assert(!defaultGetterDenot.isOverloaded, i"found overloaded default getter $defaultGetterDenot")
    val defaultGetter = defaultGetterDenot.symbol

    val targetTree = {
      if (sym.isClassConstructor || static) {
        if (targetSym.isStatic) {
          assert(captures.isEmpty, i"expected empty captures for ${targetSym.fullName} at $pos")
          genLoadModule(targetSym)
        } else {
          assert(captures.sizeIs == 1, "expected exactly one capture")

          // Find the module accessor. We cannot use memberBasedOnFlags because of scala-js/scala-js#4526.
          val outer = targetSym.originalOwner
          val name = atPhase(typerPhase)(targetSym.name.unexpandedName).sourceModuleName
          val modAccessor = outer.info.allMembers.find { denot =>
            denot.symbol.is(Module) && denot.name.unexpandedName == name
          }.getOrElse {
            ctx.implode(i"could not find module accessor for ${targetSym.fullName} at $pos")
          }.symbol

          val receiver = captures.head
          if (outer.isJSType)
            genApplyJSClassMethod(receiver, modAccessor, Nil)
          else
            genApplyMethodMaybeStatically(receiver, modAccessor, Nil)
        }
      } else {
        js.This()(currentThisType)
      }
    }

    // Pass previous arguments to defaultGetter
    val defaultGetterArgs = previousArgsValues(defaultGetter.info.paramInfoss.head.size)

    val callGetter = if (targetSym.isJSType) {
      if (defaultGetter.owner.isNonNativeJSClass) {
        if (defaultGetter.hasAnnotation(jsdefn.JSOptionalAnnot))
          js.Undefined()
        else
          genApplyJSClassMethod(targetTree, defaultGetter, defaultGetterArgs)
      } else if (defaultGetter.owner == targetSym) {
        /* We get here if a non-native constructor has a native companion.
         * This is reported on a per-class level.
         */
        assert(sym.isClassConstructor,
            s"got non-constructor method $sym with default method in JS native companion")
        js.Undefined()
      } else {
        report.error(
            "When overriding a native method with default arguments, " +
            "the overriding method must explicitly repeat the default arguments.",
            sym.srcPos)
        js.Undefined()
      }
    } else {
      genApplyMethod(targetTree, defaultGetter, defaultGetterArgs)
    }

    // #15419 If the getter returns void, we must "box" it by returning undefined
    if (callGetter.tpe == jstpe.NoType)
      js.Block(callGetter, js.Undefined())
    else
      callGetter
  }

  private def targetSymForDefaultGetter(sym: Symbol): Symbol =
    if (sym.isClassConstructor) sym.owner.companionModule.moduleClass
    else sym.owner

  private def defaultGetterDenot(targetSym: Symbol, sym: Symbol, paramIndex: Int): Denotation =
    targetSym.info.memberBasedOnFlags(DefaultGetterName(sym.name.asTermName, paramIndex), excluded = Bridge)

  private def defaultGetterDenot(sym: Symbol, paramIndex: Int): Denotation =
    defaultGetterDenot(targetSymForDefaultGetter(sym), sym, paramIndex)

  /** Generate the final forwarding call to the exported method. */
  private def genResult(exported: Exported, args: List[js.Tree], static: Boolean)(
      implicit pos: SourcePosition): js.Tree = {

    val sym = exported.sym
    val currentClass = currentClassSym.get

    def receiver =
      if (static) genLoadModule(sym.owner)
      else js.This()(currentThisType)

    def boxIfNeeded(call: js.Tree): js.Tree =
      box(call, atPhase(elimErasedValueTypePhase)(sym.info.resultType))

    if (currentClass.isNonNativeJSClass) {
      assert(sym.owner == currentClass, sym.fullName)
      boxIfNeeded(genApplyJSClassMethod(receiver, sym, args))
    } else {
      if (sym.isClassConstructor)
        js.New(encodeClassName(currentClass), encodeMethodSym(sym), args)
      else if (sym.isPrivate)
        boxIfNeeded(genApplyMethodStatically(receiver, sym, args))
      else
        boxIfNeeded(genApplyMethod(receiver, sym, args))
    }
  }

  private def genThrowTypeError(msg: String = "No matching overload")(implicit pos: Position): js.Tree =
    js.Throw(js.JSNew(js.JSGlobalRef("TypeError"), js.StringLiteral(msg) :: Nil))

  abstract class Exported(
    val sym: Symbol,
    // Parameters participating in overload resolution.
    val params: scala.collection.immutable.IndexedSeq[JSParamInfo]
  ) {
    assert(!params.exists(_.capture), "illegal capture params in Exported")

    private val paramsHasDefault = {
      if (!atPhase(elimRepeatedPhase)(sym.hasDefaultParams)) {
        Vector.empty
      } else {
        val targetSym = targetSymForDefaultGetter(sym)
        params.indices.map(i => defaultGetterDenot(targetSym, sym, i).exists)
      }
    }

    def hasDefaultAt(paramIndex: Int): Boolean =
      paramIndex < paramsHasDefault.size && paramsHasDefault(paramIndex)

    val hasRepeatedParam = params.nonEmpty && params.last.repeated

    val minArgc = {
      // Find the first default param or repeated param
      params
        .indices
        .find(i => hasDefaultAt(i) || params(i).repeated)
        .getOrElse(params.size)
    }

    val maxNonRepeatedArgc = if (hasRepeatedParam) params.size - 1 else params.size

    def pos: SourcePosition = sym.sourcePos

    def exportArgTypeAt(paramIndex: Int): Type = {
      if (paramIndex < params.length) {
        params(paramIndex).info
      } else {
        assert(hasRepeatedParam, i"$sym does not have varargs nor enough params for $paramIndex")
        params.last.info
      }
    }

    def typeInfo: String = sym.info.toString

    def genBody(formalArgsRegistry: FormalArgsRegistry): js.Tree
  }

  private class ExportedSymbol(sym: Symbol, static: Boolean)
      extends Exported(sym, sym.jsParamInfos.toIndexedSeq) {

    def genBody(formalArgsRegistry: FormalArgsRegistry): js.Tree =
      genApplyForSingleExported(formalArgsRegistry, this, static)
  }

  // !!! Hash codes of RTTypeTest are meaningless because of InstanceOfTypeTest
  private sealed abstract class RTTypeTest

  private case class PrimitiveTypeTest(tpe: jstpe.Type, rank: Int) extends RTTypeTest

  // !!! This class does not have a meaningful hash code
  private case class InstanceOfTypeTest(tpe: Type) extends RTTypeTest {
    override def equals(that: Any): Boolean = {
      that match {
        case InstanceOfTypeTest(thatTpe) => tpe =:= thatTpe
        case _                           => false
      }
    }
  }

  private case object NoTypeTest extends RTTypeTest

  /** Very simple O(n²) topological sort for elements assumed to be distinct. */
  private def topoSortDistinctsWith[A <: AnyRef](coll: List[A])(lteq: (A, A) => Boolean): List[A] = {
    @tailrec
    def loop(coll: List[A], acc: List[A]): List[A] = {
      if (coll.isEmpty) acc
      else if (coll.tail.isEmpty) coll.head :: acc
      else {
        val (lhs, rhs) = coll.span(x => !coll.forall(y => (x eq y) || !lteq(x, y)))
        assert(!rhs.isEmpty, s"cycle while ordering $coll")
        loop(lhs ::: rhs.tail, rhs.head :: acc)
      }
    }

    loop(coll, Nil)
  }

  private def typeTestForTpe(tpe: Type): RTTypeTest = {
    tpe match {
      case tpe: ErasedValueType =>
        InstanceOfTypeTest(tpe.tycon.typeSymbol.typeRef)

      case _ =>
        import org.scalajs.ir.Names

        (toIRType(tpe): @unchecked) match {
          case jstpe.AnyType => NoTypeTest

          case jstpe.NoType      => PrimitiveTypeTest(jstpe.UndefType, 0)
          case jstpe.BooleanType => PrimitiveTypeTest(jstpe.BooleanType, 1)
          case jstpe.CharType    => PrimitiveTypeTest(jstpe.CharType, 2)
          case jstpe.ByteType    => PrimitiveTypeTest(jstpe.ByteType, 3)
          case jstpe.ShortType   => PrimitiveTypeTest(jstpe.ShortType, 4)
          case jstpe.IntType     => PrimitiveTypeTest(jstpe.IntType, 5)
          case jstpe.LongType    => PrimitiveTypeTest(jstpe.LongType, 6)
          case jstpe.FloatType   => PrimitiveTypeTest(jstpe.FloatType, 7)
          case jstpe.DoubleType  => PrimitiveTypeTest(jstpe.DoubleType, 8)

          case jstpe.ClassType(Names.BoxedUnitClass)   => PrimitiveTypeTest(jstpe.UndefType, 0)
          case jstpe.ClassType(Names.BoxedStringClass) => PrimitiveTypeTest(jstpe.StringType, 9)
          case jstpe.ClassType(_)                      => InstanceOfTypeTest(tpe)

          case jstpe.ArrayType(_) => InstanceOfTypeTest(tpe)
        }
    }
  }

  // Group-by that does not rely on hashCode(), only equals() - O(n²)
  private def groupByWithoutHashCode[A, B](coll: List[A])(f: A => B): List[(B, List[A])] = {
    val m = new mutable.ArrayBuffer[(B, List[A])]
    m.sizeHint(coll.length)

    for (elem <- coll) {
      val key = f(elem)
      val index = m.indexWhere(_._1 == key)
      if (index < 0)
        m += ((key, List(elem)))
      else
        m(index) = (key, elem :: m(index)._2)
    }

    m.toList
  }

  class FormalArgsRegistry(val minArgc: Int, needsRestParam: Boolean) {
    private val fixedParamNames: scala.collection.immutable.IndexedSeq[jsNames.LocalName] =
      (0 until minArgc).toIndexedSeq.map(_ => freshLocalIdent("arg")(NoPosition).name)

    private val restParamName: jsNames.LocalName =
      if (needsRestParam) freshLocalIdent("rest")(NoPosition).name
      else null

    def genFormalArgs()(implicit pos: Position): (List[js.ParamDef], Option[js.ParamDef]) = {
      val fixedParamDefs = fixedParamNames.toList.map { paramName =>
        js.ParamDef(js.LocalIdent(paramName), NoOriginalName, jstpe.AnyType, mutable = false)
      }

      val restParam = {
        if (needsRestParam)
          Some(js.ParamDef(js.LocalIdent(restParamName), NoOriginalName, jstpe.AnyType, mutable = false))
        else
          None
      }

      (fixedParamDefs, restParam)
    }

    def genArgRef(index: Int)(implicit pos: Position): js.Tree = {
      if (index < minArgc)
        js.VarRef(js.LocalIdent(fixedParamNames(index)))(jstpe.AnyType)
      else
        js.JSSelect(genRestArgRef(), js.IntLiteral(index - minArgc))
    }

    def genVarargRef(fixedParamCount: Int)(implicit pos: Position): js.Tree = {
      assert(fixedParamCount >= minArgc, s"genVarargRef($fixedParamCount) with minArgc = $minArgc at $pos")
      val restParam = genRestArgRef()
      if (fixedParamCount == minArgc)
        restParam
      else
        js.JSMethodApply(restParam, js.StringLiteral("slice"), List(js.IntLiteral(fixedParamCount - minArgc)))
    }

    def genRestArgRef()(implicit pos: Position): js.Tree = {
      assert(needsRestParam, s"trying to generate a reference to non-existent rest param at $pos")
      js.VarRef(js.LocalIdent(restParamName))(jstpe.AnyType)
    }

    def genAllArgsRefsForForwarder()(implicit pos: Position): List[js.TreeOrJSSpread] = {
      val fixedArgRefs = fixedParamNames.toList.map { paramName =>
        js.VarRef(js.LocalIdent(paramName))(jstpe.AnyType)
      }

      if (needsRestParam) {
        val restArgRef = js.VarRef(js.LocalIdent(restParamName))(jstpe.AnyType)
        fixedArgRefs :+ js.JSSpread(restArgRef)
      } else {
        fixedArgRefs
      }
    }
  }
}
