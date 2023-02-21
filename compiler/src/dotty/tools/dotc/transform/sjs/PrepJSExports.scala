package dotty.tools.dotc
package transform
package sjs

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core._
import Contexts._
import Decorators._
import Denotations._
import Flags._
import NameKinds.DefaultGetterName
import StdNames._
import Symbols._
import SymUtils._
import Types._

import util.Spans.Span
import util.SrcPos

import dotty.tools.backend.sjs.JSDefinitions.jsdefn
import JSExportUtils._
import JSSymUtils._

import org.scalajs.ir.Names.DefaultModuleID
import org.scalajs.ir.Trees.TopLevelExportDef.isValidTopLevelExportName

object PrepJSExports {
  import tpd._
  import PrepJSInterop.{checkSetterSignature, isJSAny, isPrivateMaybeWithin}

  private sealed abstract class ExportDestination

  private object ExportDestination {
    /** Export in the "normal" way: as an instance member, or at the top-level
     *  for naturally top-level things (classes and modules).
     */
    case object Normal extends ExportDestination

    /** Export at the top-level. */
    final case class TopLevel(moduleID: String) extends ExportDestination

    /** Export as a static member of the companion class. */
    case object Static extends ExportDestination
  }

  private final case class ExportInfo(jsName: String, destination: ExportDestination)(val pos: SrcPos)

  /** Checks a class or module class for export.
   *
   *  Note that non-module Scala classes are never actually exported; their constructors are.
   *  However, the checks are performed on the class when the class is annotated.
   */
  def checkClassOrModuleExports(sym: Symbol)(using Context): Unit = {
    val exports = exportsOf(sym)
    if (exports.nonEmpty)
      checkClassOrModuleExports(sym, exports.head.pos)
  }

  /** Generate the exporter for the given DefDef or ValDef.
   *
   *  If this DefDef is a constructor, it is registered to be exported by
   *  GenJSCode instead and no trees are returned.
   */
  def genExportMember(baseSym: Symbol)(using Context): List[Tree] = {
    val clsSym = baseSym.owner

    val exports = exportsOf(baseSym)

    // Helper function for errors
    def err(msg: String): List[Tree] = {
      report.error(msg, exports.head.pos)
      Nil
    }

    def memType = if (baseSym.isConstructor) "constructor" else "method"

    if (exports.isEmpty) {
      Nil
    } else if (!hasLegalExportVisibility(baseSym)) {
      err(s"You may only export public and protected ${memType}s")
    } else if (baseSym.is(Inline)) {
      err("You may not export an inline method")
    } else if (isJSAny(clsSym)) {
      err(s"You may not export a $memType of a subclass of js.Any")
    } else if (baseSym.isLocalToBlock) {
      err("You may not export a local definition")
    } else if (hasIllegalRepeatedParam(baseSym)) {
      err(s"In an exported $memType, a *-parameter must come last (through all parameter lists)")
    } else if (hasIllegalDefaultParam(baseSym)) {
      err(s"In an exported $memType, all parameters with defaults must be at the end")
    } else if (baseSym.isConstructor) {
      // Constructors do not need an exporter method. We only perform the checks at this phase.
      checkClassOrModuleExports(clsSym, exports.head.pos)
      Nil
    } else {
      assert(!baseSym.is(Bridge), s"genExportMember called for bridge symbol $baseSym")
      val normalExports = exports.filter(_.destination == ExportDestination.Normal)
      normalExports.flatMap(exp => genExportDefs(baseSym, exp.jsName, exp.pos.span))
    }
  }

  /** Check a class or module for export.
   *
   *  There are 2 ways that this method can be reached:
   *  - via `registerClassExports`
   *  - via `genExportMember` (constructor of Scala class)
   */
  private def checkClassOrModuleExports(sym: Symbol, errPos: SrcPos)(using Context): Unit = {
    val isMod = sym.is(ModuleClass)

    def err(msg: String): Unit =
      report.error(msg, errPos)

    def hasAnyNonPrivateCtor: Boolean =
      sym.info.decl(nme.CONSTRUCTOR).hasAltWith(denot => !isPrivateMaybeWithin(denot.symbol))

    if (sym.is(Trait)) {
      err("You may not export a trait")
    } else if (sym.hasAnnotation(jsdefn.JSNativeAnnot)) {
      err("You may not export a native JS " + (if (isMod) "object" else "class"))
    } else if (!hasLegalExportVisibility(sym)) {
      err("You may only export public and protected " + (if (isMod) "objects" else "classes"))
    } else if (isJSAny(sym.owner)) {
      err("You may not export a " + (if (isMod) "object" else "class") + " in a subclass of js.Any")
    } else if (sym.isLocalToBlock) {
      err("You may not export a local " + (if (isMod) "object" else "class"))
    } else if (!sym.isStatic) {
      if (isMod)
        err("You may not export a nested object")
      else
        err("You may not export a nested class. Create an exported factory method in the outer class to work around this limitation.")
    } else if (sym.is(Abstract, butNot = Trait) && !isJSAny(sym)) {
      err("You may not export an abstract class")
    } else if (!isMod && !hasAnyNonPrivateCtor) {
      /* This test is only relevant for JS classes but doesn't hurt for Scala
       * classes as we could not reach it if there were only private
       * constructors.
       */
      err("You may not export a class that has only private constructors")
    } else {
      // OK
    }
  }

  /** Computes the ExportInfos for sym from its annotations. */
  private def exportsOf(sym: Symbol)(using Context): List[ExportInfo] = {
    val trgSym = {
      def isOwnerScalaClass = !sym.owner.is(ModuleClass) && !isJSAny(sym.owner)

      // For primary Scala class constructors, look on the class itself
      if (sym.isPrimaryConstructor && isOwnerScalaClass) sym.owner
      else sym
    }

    val JSExportAnnot = jsdefn.JSExportAnnot
    val JSExportTopLevelAnnot = jsdefn.JSExportTopLevelAnnot
    val JSExportStaticAnnot = jsdefn.JSExportStaticAnnot
    val JSExportAllAnnot = jsdefn.JSExportAllAnnot

    // Annotations that are directly on the member
    val directMemberAnnots = Set[Symbol](JSExportAnnot, JSExportTopLevelAnnot, JSExportStaticAnnot)
    val directAnnots = trgSym.annotations.filter(annot => directMemberAnnots.contains(annot.symbol))

    // Is this a member export (i.e. not a class or module export)?
    val isMember = !sym.isClass && !sym.isConstructor

    // Annotations for this member on the whole unit
    val unitAnnots = {
      if (isMember && sym.isPublic && !sym.is(Synthetic))
        sym.owner.annotations.filter(_.symbol == JSExportAllAnnot)
      else
        Nil
    }

    val allExportInfos = for {
      annot <- directAnnots ++ unitAnnots
    } yield {
      val isExportAll = annot.symbol == JSExportAllAnnot
      val isTopLevelExport = annot.symbol == JSExportTopLevelAnnot
      val isStaticExport = annot.symbol == JSExportStaticAnnot
      val hasExplicitName = annot.arguments.nonEmpty

      val exportPos: SrcPos = if (isExportAll) sym else annot.tree

      assert(!isTopLevelExport || hasExplicitName,
          em"Found a top-level export without an explicit name at ${exportPos.sourcePos}")

      val name = {
        if (hasExplicitName) {
          annot.argumentConstantString(0).getOrElse {
            report.error(
                em"The argument to ${annot.symbol.name} must be a literal string",
                annot.arguments(0))
            "dummy"
          }
        } else {
          sym.defaultJSName
        }
      }

      val destination = {
        if (isTopLevelExport) {
          val moduleID = if (annot.arguments.size == 1) {
            DefaultModuleID
          } else {
            annot.argumentConstantString(1).getOrElse {
              report.error("moduleID must be a literal string", annot.arguments(1))
              DefaultModuleID
            }
          }

          ExportDestination.TopLevel(moduleID)
        } else if (isStaticExport) {
          ExportDestination.Static
        } else {
          ExportDestination.Normal
        }
      }

      // Enforce proper setter signature
      if (sym.isJSSetter)
        checkSetterSignature(sym, exportPos, exported = true)

      // Enforce no __ in name
      if (!isTopLevelExport && name.contains("__"))
        report.error("An exported name may not contain a double underscore (`__`)", exportPos)

      /* Illegal function application exports, i.e., method named 'apply'
       * without an explicit export name.
       */
      if (isMember && !hasExplicitName && sym.name == nme.apply) {
        destination match {
          case ExportDestination.Normal =>
            def shouldBeTolerated = {
              isExportAll && directAnnots.exists { annot =>
                annot.symbol == JSExportAnnot &&
                annot.arguments.nonEmpty &&
                annot.argumentConstantString(0).contains("apply")
              }
            }

            // Don't allow apply without explicit name
            if (!shouldBeTolerated) {
              report.error(
                  "A member cannot be exported to function application. " +
                  "Add @JSExport(\"apply\") to export under the name apply.",
                  exportPos)
            }

          case _: ExportDestination.TopLevel =>
            ctx.implode(
                em"Found a top-level export without an explicit name at ${exportPos.sourcePos}")

          case ExportDestination.Static =>
            report.error(
                "A member cannot be exported to function application as static. " +
                "Use @JSExportStatic(\"apply\") to export it under the name 'apply'.",
                exportPos)
        }
      }

      val symOwner =
        if (sym.isConstructor) sym.owner.owner
        else sym.owner

      // Destination-specific restrictions
      destination match {
        case ExportDestination.Normal =>
          // Make sure we do not override the default export of toString
          def isIllegalToString = {
            isMember && name == "toString" && sym.name != nme.toString_ &&
            sym.info.paramInfoss.forall(_.isEmpty) && !sym.isJSGetter
          }
          if (isIllegalToString) {
            report.error(
                "You may not export a zero-argument method named other than 'toString' under the name 'toString'",
                exportPos)
          }

          // Disallow @JSExport at the top-level, as well as on objects and classes
          if (symOwner.is(Package) || symOwner.isPackageObject) {
            report.error("@JSExport is forbidden on top-level definitions. Use @JSExportTopLevel instead.", exportPos)
          } else if (!isMember && !sym.is(Trait)) {
            report.error(
                "@JSExport is forbidden on objects and classes. Use @JSExport'ed factory methods instead.",
                exportPos)
          }

        case _: ExportDestination.TopLevel =>
          if (sym.is(Lazy))
            report.error("You may not export a lazy val to the top level", exportPos)
          else if (sym.is(Method, butNot = Accessor) && sym.isJSProperty)
            report.error("You may not export a getter or a setter to the top level", exportPos)

          /* Disallow non-static methods.
           * Note: Non-static classes have more specific error messages in checkClassOrModuleExports.
           */
          if (sym.isTerm && (!symOwner.isStatic || !symOwner.is(ModuleClass)))
            report.error("Only static objects may export their members to the top level", exportPos)

          // The top-level name must be a valid JS identifier
          if (!isValidTopLevelExportName(name))
            report.error("The top-level export name must be a valid JavaScript identifier name", exportPos)

        case ExportDestination.Static =>
          def companionIsNonNativeJSClass: Boolean = {
            val companion = symOwner.companionClass
            companion != NoSymbol
              && !companion.is(Trait)
              && isJSAny(companion)
              && !companion.hasAnnotation(jsdefn.JSNativeAnnot)
          }

          if (!symOwner.isStatic || !symOwner.is(ModuleClass) || !companionIsNonNativeJSClass) {
            report.error(
                "Only a static object whose companion class is a non-native JS class may export its members as static.",
                exportPos)
          }

          if (isMember) {
            if (sym.is(Lazy))
              report.error("You may not export a lazy val as static", exportPos)
          } else {
            if (sym.is(Trait))
              report.error("You may not export a trait as static.", exportPos)
            else
              report.error("Implementation restriction: cannot export a class or object as static", exportPos)
          }
      }

      ExportInfo(name, destination)(exportPos)
    }

    allExportInfos.filter(_.destination == ExportDestination.Normal)
      .groupBy(_.jsName)
      .filter { case (jsName, group) =>
        if (jsName == "apply" && group.size == 2)
          // @JSExportAll and single @JSExport("apply") should not be warned.
          !unitAnnots.exists(_.symbol == JSExportAllAnnot)
        else
          group.size > 1
      }
      .foreach(_ => report.warning("Found duplicate @JSExport", sym))

    /* Make sure that no field is exported *twice* as static, nor both as
     * static and as top-level (it is possible to export a field several times
     * as top-level, though).
     */
    if (!sym.is(Method)) {
      for (firstStatic <- allExportInfos.find(_.destination == ExportDestination.Static)) {
        for (duplicate <- allExportInfos) {
          duplicate.destination match {
            case ExportDestination.Normal =>
              // OK
            case ExportDestination.Static =>
              if (duplicate ne firstStatic) {
                report.error(
                    "Fields (val or var) cannot be exported as static more than once",
                    duplicate.pos)
              }
            case _: ExportDestination.TopLevel =>
              report.error(
                  "Fields (val or var) cannot be exported both as static and at the top-level",
                  duplicate.pos)
          }
        }
      }
    }

    allExportInfos.distinct
  }

  /** Generates an exporter for a DefDef including default parameter methods. */
  private def genExportDefs(defSym: Symbol, jsName: String, span: Span)(using Context): List[Tree] = {
    val clsSym = defSym.owner.asClass

    // Create symbol for new method
    val name = makeExportName(jsName, !defSym.is(Method) || defSym.isJSProperty)
    val flags = (defSym.flags | Method | Synthetic)
      &~ (Deferred | Accessor | ParamAccessor | CaseAccessor | Mutable | Lazy | Override)
    val info =
      if (defSym.isConstructor) defSym.info
      else if (defSym.is(Method)) finalResultTypeToAny(defSym.info)
      else ExprType(defn.AnyType)
    val expSym = newSymbol(clsSym, name, flags, info, defSym.privateWithin, span).entered

    // Construct exporter DefDef tree
    val exporter = genProxyDefDef(clsSym, defSym, expSym, span)

    // Construct exporters for default getters
    val defaultGetters = if (!defSym.hasDefaultParams) {
      Nil
    } else {
      for {
        (param, i) <- defSym.paramSymss.flatten.zipWithIndex
        if param.is(HasDefault)
      } yield {
        genExportDefaultGetter(clsSym, defSym, expSym, i, span)
      }
    }

    exporter :: defaultGetters
  }

  private def genExportDefaultGetter(clsSym: ClassSymbol, trgMethod: Symbol,
      exporter: Symbol, paramPos: Int, span: Span)(using Context): Tree = {

    // Get default getter method we'll copy
    val trgGetterDenot = defaultGetterDenot(clsSym, trgMethod, paramPos)

    assert(trgGetterDenot.exists, em"Cannot find default getter for param $paramPos of $trgMethod")

    // Although the following must be true in a correct program, we cannot
    // assert, since a graceful failure message is only generated later
    if (!trgGetterDenot.isOverloaded) {
      val trgGetter = trgGetterDenot.symbol
      val expGetterName = DefaultGetterName(exporter.name.asTermName, paramPos)
      val expGetter = newSymbol(clsSym, expGetterName, trgGetter.flags, trgGetter.info,
          trgGetter.privateWithin, coord = span).entered
      genProxyDefDef(clsSym, trgGetter, expGetter, span)
    } else {
      EmptyTree
    }
  }

  private def defaultGetterDenot(targetSym: Symbol, sym: Symbol, paramIndex: Int)(using Context): Denotation =
    targetSym.info.member(DefaultGetterName(sym.name.asTermName, paramIndex))

  /** generate a DefDef tree (from [[proxySym]]) that calls [[trgSym]] */
  private def genProxyDefDef(clsSym: ClassSymbol, trgSym: Symbol,
      proxySym: TermSymbol, span: Span)(using Context): Tree = {

    DefDef(proxySym, { argss =>
      This(clsSym).select(trgSym).appliedToArgss(argss)
    }).withSpan(span)
  }

  /** Changes the final result type of a type `tpe` to Any. */
  private def finalResultTypeToAny(tpe: Type)(using Context): Type = tpe match {
    case tpe: MethodType =>
      MethodType(tpe.paramNames, tpe.paramInfos, finalResultTypeToAny(tpe.resultType))
    case _: ExprType =>
      ExprType(defn.AnyType)
    case tpe: PolyType =>
      PolyType(tpe.paramNames)(
          x => tpe.paramInfos.mapConserve(_.subst(tpe, x).bounds),
          x => finalResultTypeToAny(tpe.resultType.subst(tpe, x)))
    case _ =>
      defn.AnyType
  }

  /** Whether the given symbol has a visibility that allows exporting */
  private def hasLegalExportVisibility(sym: Symbol)(using Context): Boolean =
    sym.isPublic || sym.is(Protected, butNot = Local)

  /** Checks whether this type has a repeated parameter elsewhere than at the end of all the params. */
  private def hasIllegalRepeatedParam(sym: Symbol)(using Context): Boolean = {
    val paramInfos = sym.info.paramInfoss.flatten
    paramInfos.nonEmpty && paramInfos.init.exists(_.isRepeatedParam)
  }

  /** Checks whether there are default parameters not at the end of the flattened parameter list. */
  private def hasIllegalDefaultParam(sym: Symbol)(using Context): Boolean = {
    sym.hasDefaultParams
      && sym.paramSymss.flatten.reverse.dropWhile(_.is(HasDefault)).exists(_.is(HasDefault))
  }
}
