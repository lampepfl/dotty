package dotty.tools.dotc
package core.classfile

import scala.language.unsafeNulls

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.util._
import dotty.tools.io.AbstractFile
import dotty.tools.tasty.TastyReader

import java.io.IOException
import java.lang.Integer.toHexString
import java.util.UUID

class ClassfileTastyUUIDParser(classfile: AbstractFile)(ictx: Context) {

  import ClassfileConstants._

  private var pool: ConstantPool = _              // the classfile's constant pool

  def checkTastyUUID(tastyUUID: UUID)(using Context): Unit = try ctx.base.reusableDataReader.withInstance { reader =>
    implicit val reader2 = reader.reset(classfile)
    parseHeader()
    this.pool = new ConstantPool
    checkTastyAttr(tastyUUID)
    this.pool =  null
  }
  catch {
    case e: RuntimeException =>
      if (ctx.debug) e.printStackTrace()
      throw new IOException(
        i"""class file ${classfile.canonicalPath} is broken, reading aborted with ${e.getClass}
           |${Option(e.getMessage).getOrElse("")}""")
  }

  private def parseHeader()(using in: DataReader): Unit = {
    val magic = in.nextInt
    if (magic != JAVA_MAGIC)
      throw new IOException(s"class file '${classfile}' has wrong magic number 0x${toHexString(magic)}, should be 0x${toHexString(JAVA_MAGIC)}")
    val minorVersion = in.nextChar.toInt
    val majorVersion = in.nextChar.toInt
    if ((majorVersion < JAVA_MAJOR_VERSION) ||
        ((majorVersion == JAVA_MAJOR_VERSION) &&
         (minorVersion < JAVA_MINOR_VERSION)))
      throw new IOException(
        s"class file '${classfile}' has unknown version $majorVersion.$minorVersion, should be at least $JAVA_MAJOR_VERSION.$JAVA_MINOR_VERSION")
  }

  private def checkTastyAttr(tastyUUID: UUID)(using ctx: Context, in: DataReader): Unit = {
    in.nextChar // jflags
    in.nextChar // nameIdx
    skipSuperclasses()
    skipMembers() // fields
    skipMembers() // methods
    val attrs = in.nextChar
    val attrbp = in.bp

    def scan(target: TypeName): Boolean = {
      in.bp = attrbp
      var i = 0
      while (i < attrs && pool.getName(in.nextChar).name.toTypeName != target) {
        val attrLen = in.nextInt
        in.skip(attrLen)
        i += 1
      }
      i < attrs
    }

    if (scan(tpnme.TASTYATTR)) {
      val attrLen = in.nextInt
      val bytes = in.nextBytes(attrLen)
      if (attrLen == 16) { // A tasty attribute with that has only a UUID (16 bytes) implies the existence of the .tasty file
        val expectedUUID =
          val reader = new TastyReader(bytes, 0, 16)
          new UUID(reader.readUncompressedLong(), reader.readUncompressedLong())
        if (expectedUUID != tastyUUID)
          report.warning(s"$classfile is out of sync with its TASTy file. Loaded TASTy file. Try cleaning the project to fix this issue", NoSourcePosition)
      }
      else
        // Before 3.0.0 we had a mode where we could embed the TASTY bytes in the classfile. This has not been supported in any stable release.
        report.error(s"Found a TASTY attribute with a length different from 16 in $classfile. This is likely a bug in the compiler. Please report.", NoSourcePosition)
    }

  }

  private def skipAttributes()(using in: DataReader): Unit = {
    val attrCount = in.nextChar
    for (i <- 0 until attrCount) {
      in.skip(2); in.skip(in.nextInt)
    }
  }

  private def skipMembers()(using in: DataReader): Unit = {
    val memberCount = in.nextChar
    for (i <- 0 until memberCount) {
      in.skip(6); skipAttributes()
    }
  }

  private def skipSuperclasses()(using in: DataReader): Unit = {
    in.skip(2) // superclass
    val ifaces = in.nextChar
    in.skip(2 * ifaces)
  }

  class ConstantPool(using in: DataReader) extends ClassfileParser.AbstractConstantPool {
    def getClassOrArrayType(index: Int)(using ctx: Context, in: DataReader): Type = throw new UnsupportedOperationException
    def getClassSymbol(index: Int)(using ctx: Context, in: DataReader): Symbol = throw new UnsupportedOperationException
    def getType(index: Int, isVarargs: Boolean)(using x$3: Context, x$4: DataReader): Type = throw new UnsupportedOperationException
  }
}
