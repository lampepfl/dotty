package dotty.tools
package dotc
package core
package tasty

import scala.language.unsafeNulls

import Contexts.*, SymDenotations.*,  Decorators.*
import dotty.tools.dotc.ast.tpd
import TastyUnpickler.*
import classfile.ClassfileParser
import Names.SimpleName
import TreeUnpickler.UnpickleMode

import dotty.tools.tasty.TastyReader
import dotty.tools.tasty.TastyFormat.{ASTsSection, PositionsSection, CommentsSection, AttributesSection}
import dotty.tools.tasty.TastyVersion

import dotty.tools.io.AbstractFile

object DottyUnpickler {

  /** Exception thrown if classfile is corrupted */
  class BadSignature(msg: String) extends RuntimeException(msg)

  class TreeSectionUnpickler(
    compilationUnitInfo: CompilationUnitInfo,
    posUnpickler: Option[PositionUnpickler],
    commentUnpickler: Option[CommentUnpickler],
    isBestEffortTasty: Boolean
  ) extends SectionUnpickler[TreeUnpickler](ASTsSection) {
    def unpickle(reader: TastyReader, nameAtRef: NameTable): TreeUnpickler =
      new TreeUnpickler(reader, nameAtRef, compilationUnitInfo, posUnpickler, commentUnpickler, isBestEffortTasty)
  }

  class PositionsSectionUnpickler extends SectionUnpickler[PositionUnpickler](PositionsSection) {
    def unpickle(reader: TastyReader, nameAtRef: NameTable): PositionUnpickler =
      new PositionUnpickler(reader, nameAtRef)
  }

  class CommentsSectionUnpickler extends SectionUnpickler[CommentUnpickler](CommentsSection) {
    def unpickle(reader: TastyReader, nameAtRef: NameTable): CommentUnpickler =
      new CommentUnpickler(reader)
  }

  class AttributesSectionUnpickler extends SectionUnpickler[AttributeUnpickler](AttributesSection) {
    def unpickle(reader: TastyReader, nameAtRef: NameTable): AttributeUnpickler =
      new AttributeUnpickler(reader, nameAtRef)
  }
}

/** A class for unpickling Tasty trees and symbols.
 *  @param tastyFile         tasty file from which we unpickle (used for CompilationUnitInfo)
 *  @param bytes             the bytearray containing the Tasty file from which we unpickle
 *  @param isBestEffortTasty specifies whether file should be unpickled as a Best Effort TASTy
 *  @param mode              the tasty file contains package (TopLevel), an expression (Term) or a type (TypeTree)
 */
class DottyUnpickler(
  tastyFile: AbstractFile,
  bytes: Array[Byte],
  isBestEffortTasty: Boolean,
  mode: UnpickleMode = UnpickleMode.TopLevel
) extends ClassfileParser.Embedded with tpd.TreeProvider {
  import tpd.*
  import DottyUnpickler.*

  val unpickler: TastyUnpickler = new TastyUnpickler(bytes, isBestEffortTasty)

  val tastyAttributes: Attributes =
    unpickler.unpickle(new AttributesSectionUnpickler)
      .map(_.attributes).getOrElse(Attributes.empty)
  val compilationUnitInfo: CompilationUnitInfo =
    import unpickler.header.{majorVersion, minorVersion, experimentalVersion}
    val tastyVersion = TastyVersion(majorVersion, minorVersion, experimentalVersion)
    val tastyInfo = TastyInfo(tastyVersion, tastyAttributes)
    new CompilationUnitInfo(tastyFile, Some(tastyInfo))

  private val posUnpicklerOpt = unpickler.unpickle(new PositionsSectionUnpickler)
  private val commentUnpicklerOpt = unpickler.unpickle(new CommentsSectionUnpickler)
  private val treeUnpickler = unpickler.unpickle(treeSectionUnpickler(posUnpicklerOpt, commentUnpicklerOpt, isBestEffortTasty)).get

  /** Enter all toplevel classes and objects into their scopes
   *  @param roots          a set of SymDenotations that should be overwritten by unpickling
   */
  def enter(roots: Set[SymDenotation])(using Context): Unit =
    treeUnpickler.enter(roots)

  protected def treeSectionUnpickler(
    posUnpicklerOpt: Option[PositionUnpickler],
    commentUnpicklerOpt: Option[CommentUnpickler],
    withBestEffortTasty: Boolean
  ): TreeSectionUnpickler =
    new TreeSectionUnpickler(compilationUnitInfo, posUnpicklerOpt, commentUnpicklerOpt, withBestEffortTasty)

  protected def computeRootTrees(using Context): List[Tree] = treeUnpickler.unpickle(mode)

  private var ids: Array[String] = null

  override def mightContain(id: String)(using Context): Boolean = {
    if (ids == null)
      ids =
        unpickler.nameAtRef.contents.toArray.collect {
          case name: SimpleName => name.toString
        }.sorted
    ids.binarySearch(id) >= 0
  }
}
