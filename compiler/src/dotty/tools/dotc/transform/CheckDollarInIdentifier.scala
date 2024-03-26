package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.report


class CheckDollarInIdentifier extends MiniPhase:
  import CheckDollarInIdentifier.ShouldNotContainDollarSignError

  override def phaseName: String = CheckDollarInIdentifier.phaseName

  override def description: String = CheckDollarInIdentifier.description

  override def isRunnable(using Context): Boolean =
    super.isRunnable &&
      ctx.settings.WdollarCheck.value &&
      !ctx.isJava

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree =
    reportDollarInIdentifier(tree)

  override def transformDefDef(tree: tpd.DefDef)(using Context): tpd.Tree =
    reportDollarInIdentifier(tree)

  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree =
    reportDollarInIdentifier(tree)

  override def transformTemplate(tree: tpd.Template)(using Context): tpd.Tree =
    reportDollarInIdentifier(tree)

  override def transformCaseDef(tree: tpd.CaseDef)(using Context): tpd.Tree =
    reportDollarInIdentifier(tree)

  override def transformPackageDef(tree: tpd.PackageDef)(using Context): tpd.Tree =
    reportDollarInIdentifier(tree)


  private val allowedValTermNames = List(
    nme.DOLLAR_VALUES,
    nme.ordinalDollar_,
  ).map(_.toString)

  private val allowedDefTermNames = List(
    nme.DOLLAR_NEW,
    nme.ANON_FUN,
  ).map(_.toString)

  private def allowedTerms[T <: Tree](tree: T): List[String] =
    tree match
      case _: tpd.ValDef => allowedValTermNames
      case _: tpd.DefDef => allowedDefTermNames
      case _ => Nil

  private def containsAllowedTerms[T <: Tree](tree: T, name: String): Boolean =
    allowedTerms(tree).contains(name)

  private def symbolName[T <: Tree](tree: T)(using Context): String =
    tree match
      case typeDef: tpd.TypeDef =>
        typeDef.symbol.toString match {
          case s"module class $className$$" => className // to catch the compiled singleton class which includes a $ at the end
          case _ => tree.symbol.name.toString
        }
      case _ => tree.symbol.name.toString

  private def reportDollarInIdentifier[T <: Tree](tree: T)(using Context): T =
    val name = symbolName(tree)
    val originalSrcPos = tree.srcPos.sourcePos
    val originalSpan = originalSrcPos.span

    // only report source-derived (non-synthetic) spans
    if name.contains("$") && !containsAllowedTerms(tree, name) && !originalSpan.isSynthetic then
      val srcPos = originalSrcPos.withSpan(originalSrcPos.span.focus) // focus span to place a single `^` at identifier position
      report.warning(ShouldNotContainDollarSignError, srcPos)

    tree


object CheckDollarInIdentifier:
  val phaseName: String = "checkDollarInIdentifier"
  val description: String = "warns if identifiers contain dollar sign, $"

  private val ShouldNotContainDollarSignError = s"identifiers should not include dollar sign"

