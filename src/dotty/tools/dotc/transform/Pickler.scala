package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import Contexts.Context
import Decorators._
import pickling._
import config.Printers

/** This miniphase pickles trees */
class Pickler extends MiniPhaseTransform { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "pickler"
  
  override def transformUnit(tree: Tree)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!ctx.compilationUnit.isJava) {
      val pickler = new TastyPickler
      new TreePickler(pickler, picklePositions = false).pickle(tree)
      val bytes = pickler.assembleParts()
      def rawBytes = // not needed right now, but useful to print raw format.
        bytes.iterator.grouped(10).toList.zipWithIndex.map {
          case (row, i) => s"${i}0: ${row.mkString(" ")}"
        }
      if (Printers.pickling ne Printers.noPrinter) new TastyPrinter(bytes).printContents()
    }
    tree
  }
}