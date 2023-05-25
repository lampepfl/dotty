package dotty.tools.dotc
package core
package tasty

import dotty.tools.tasty.{TastyBuffer, TastyReader}
import TastyBuffer.NameRef

import Contexts.*, Decorators.*
import Names.TermName
import StdNames.nme
import TastyUnpickler.*
import dotty.tools.tasty.TastyFormat.ASTsSection

/** Reads the package and class name of the class contained in this TASTy */
class TastyClassName(bytes: Array[Byte], isBestEffortTasty: Boolean = false) {

  val unpickler: TastyUnpickler = new TastyUnpickler(bytes, isBestEffortTasty)
  import unpickler.{nameAtRef, unpickle}

  /** Returns a tuple with the package and class names */
  def readName(): Option[(TermName, TermName)] = unpickle(new TreeSectionUnpickler)

  class TreeSectionUnpickler extends SectionUnpickler[(TermName, TermName)](ASTsSection) {
    import dotty.tools.tasty.TastyFormat.*
    def unpickle(reader: TastyReader, tastyName: NameTable): (TermName, TermName) = {
      import reader.*
      def readNames(packageName: TermName): (TermName, TermName) = {
        val tag = readByte()
        if (tag >= firstLengthTreeTag) {
          val len = readNat()
          val end = currentAddr + len
          tag match {
            case TYPEDEF =>
              val className = reader.readName()
              goto(end)
              (packageName, className)
            case IMPORT | VALDEF =>
              goto(end)
              readNames(packageName)
            case PACKAGE =>
              readNames(packageName)
          }
        }
        else tag match {
          case TERMREFpkg | TYPEREFpkg =>
            val subPackageName = reader.readName()
            readNames(subPackageName)
          case SHAREDtype =>
            val addr = reader.readAddr()
            val reader2 = reader.subReader(addr, reader.endAddr)
            val tag2 = reader2.readByte()
            assert(tag2 == TERMREFpkg || tag2 == TYPEREFpkg)
            val subPackageName = reader2.readName()
            readNames(subPackageName)
          case _ =>
            readNames(packageName)
        }
      }
      readNames(nme.EMPTY_PACKAGE)
    }

    extension (reader: TastyReader) def readName() = {
      val idx = reader.readNat()
      nameAtRef(NameRef(idx))
    }
  }

}
