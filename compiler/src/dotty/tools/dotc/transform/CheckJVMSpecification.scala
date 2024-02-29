package dotty.tools.dotc
package transform

import MegaPhase.MiniPhase
import GenericSignatures.*
import core.Contexts.*
import core.Decorators.*
import core.Constants.*
import core.Symbols.*
import util.SrcPos

final class CheckJVMSpecification extends MiniPhase:
  import ast.tpd.*

  override def phaseName: String = CheckJVMSpecification.name
  override def description: String = CheckJVMSpecification.description


  override def transformDefDef(tree: DefDef)(using Context): Tree =
    val sym = tree.symbol
    val memberTpe = atPhase(ctx.base.erasurePhase) { sym.owner.denot.thisType.memberInfo(sym) }
    for sig <- GenericSignatures.javaSig(sym, memberTpe) do
      checkUTF8MaximumLength(sig, tree.srcPos, prefix = "Method signature")
    tree

  override def transformLiteral(tree: Literal)(using Context): Tree =
    val Literal(cste) = tree
    if cste.tpe == defn.StringType then
      checkUTF8MaximumLength(cste.stringValue, tree.srcPos.focus, prefix = "String")
    tree

  /** See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4.7 */
  private def checkUTF8MaximumLength(str: String, pos: SrcPos, prefix: String)(using Context) =
    val UTF8_MAX_LENGTH = 65535
    if str.length > UTF8_MAX_LENGTH then
      report.error(em"""${prefix} length exceed the maximum length allowed by the JVM specification.
                        |Allowed: ${UTF8_MAX_LENGTH}
                        |Actual : ${str.length}""", pos)

object CheckJVMSpecification:
  val name = "checkJVMspec"
  val description = "check issues related to the jvm specification"

