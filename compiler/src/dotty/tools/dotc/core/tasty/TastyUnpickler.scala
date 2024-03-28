package dotty.tools.dotc
package core
package tasty

import java.util.UUID
import scala.language.unsafeNulls

import dotty.tools.tasty.{TastyFormat, TastyVersion, TastyBuffer, TastyReader, TastyHeaderUnpickler, UnpicklerConfig}
import dotty.tools.tasty.besteffort.{BestEffortTastyHeader, BestEffortTastyHeaderUnpickler}

import TastyFormat.NameTags.*, TastyFormat.nameTagToString
import TastyBuffer.NameRef

import scala.collection.mutable
import Names.{TermName, termName, EmptyTermName}
import NameKinds.*
import dotty.tools.tasty.TastyHeader
import dotty.tools.tasty.TastyBuffer.Addr

case class CommonTastyHeader(
  uuid: UUID,
  majorVersion: Int,
  minorVersion: Int,
  experimentalVersion: Int,
  toolingVersion: String
):
  def this(h: TastyHeader) =
    this(h.uuid, h.majorVersion, h.minorVersion, h.experimentalVersion, h.toolingVersion)
  def this(h: BestEffortTastyHeader) =
    this(h.uuid, h.majorVersion, h.minorVersion, h.experimentalVersion, h.toolingVersion)

object TastyUnpickler {

  abstract class SectionUnpickler[R](val name: String) {
    def unpickle(reader: TastyReader, nameAtRef: NameTable): R
  }

  class NameTable extends (NameRef => TermName) {
    private val names = new mutable.ArrayBuffer[TermName]
    def add(name: TermName): mutable.ArrayBuffer[TermName] = names += name
    def apply(ref: NameRef): TermName = names(ref.index)
    def contents: Iterable[TermName] = names
  }

  trait Scala3CompilerConfig extends UnpicklerConfig:
    private def asScala3Compiler(version: TastyVersion): String =
      if (version.major == 28) {
        // scala 3.x.y series
        if (version.experimental > 0)
          // scenario here is someone using 3.4.0 to read 3.4.1-RC1-NIGHTLY, in this case, we should show 3.4 nightly.
          s"the same nightly or snapshot Scala 3.${version.minor - 1} compiler"
        else s"a Scala 3.${version.minor}.0 compiler or newer"
      }
      else if (version.experimental > 0) "the same Scala compiler" // unknown major version, just say same
      else "a more recent Scala compiler" // unknown major version, just say later

    /** The description of the upgraded scala compiler that can read the given TASTy version */
    final def upgradedReaderTool(version: TastyVersion): String = asScala3Compiler(version)

    /** The description of the upgraded scala compiler that can produce the given TASTy version */
    final def upgradedProducerTool(version: TastyVersion): String = asScala3Compiler(version)

    final def recompileAdditionalInfo: String = """
      |  Usually this means that the library dependency containing this file should be updated.""".stripMargin

    final def upgradeAdditionalInfo(fileVersion: TastyVersion): String =
      if (fileVersion.isExperimental && experimentalVersion == 0) {
        """
          |  Note that you are using a stable compiler, which can not read experimental TASTy.""".stripMargin
      }
      else ""
  end Scala3CompilerConfig

  /** A config for the TASTy reader of a scala 3 compiler */
  val scala3CompilerConfig: UnpicklerConfig = new Scala3CompilerConfig with UnpicklerConfig.DefaultTastyVersion {}

}

import TastyUnpickler.*

class TastyUnpickler(protected val reader: TastyReader, isBestEffortTasty: Boolean) {
  import reader.*

  def this(bytes: Array[Byte]) = this(new TastyReader(bytes), false)
  def this(bytes: Array[Byte], isBestEffortTasty: Boolean) = this(new TastyReader(bytes), isBestEffortTasty)

  private val sectionReader = new mutable.HashMap[String, TastyReader]
  val nameAtRef: NameTable = new NameTable

  private def readName(): TermName = nameAtRef(readNameRef())
  private def readString(): String = readName().toString

  private def readParamSig(): Signature.ParamSig = {
    val ref = readInt()
    if (ref < 0)
      ref.abs
    else
      nameAtRef(NameRef(ref)).toTypeName
  }

  private def readNameContents(): TermName = {
    val tag = readByte()
    val length = readNat()
    val start = currentAddr
    val end = start + length
    def readSignedRest(original: TermName, target: TermName): TermName =
      val result = readName().toTypeName
      val paramsSig = until(end)(readParamSig())
      val sig = Signature(paramsSig, result)
      SignedName(original, sig, target)

    val result = tag match {
      case UTF8 =>
        goto(end)
        termName(bytes, start.index, length)
      case QUALIFIED | EXPANDED | EXPANDPREFIX =>
        qualifiedNameKindOfTag(tag)(readName(), readName().asSimpleName)
      case UNIQUE =>
        val separator = readName().toString
        val num = readNat()
        val originals = until(end)(readName())
        val original = if (originals.isEmpty) EmptyTermName else originals.head
        uniqueNameKindOfSeparator(separator)(original, num)
      case DEFAULTGETTER =>
        numberedNameKindOfTag(tag)(readName(), readNat())
      case SIGNED =>
        val original = readName()
        readSignedRest(original, original)
      case TARGETSIGNED =>
        val original = readName()
        val target = readName()
        readSignedRest(original, target)
      case SUPERACCESSOR | INLINEACCESSOR | BODYRETAINER | OBJECTCLASS =>
        simpleNameKindOfTag(tag)(readName())
      case _ =>
        throw MatchError(s"unknown name tag ${nameTagToString(tag)}")
    }
    assert(currentAddr == end, s"bad name $result $start $currentAddr $end")
    result
  }

  val header: CommonTastyHeader =
    if isBestEffortTasty then
      new CommonTastyHeader(new BestEffortTastyHeaderUnpickler(scala3CompilerConfig, reader).readFullHeader())
    else
      new CommonTastyHeader(new TastyHeaderUnpickler(reader).readFullHeader())

  def readNames(): Unit =
    until(readEnd()) { nameAtRef.add(readNameContents()) }

  def loadSections(): Unit = {
    while (!isAtEnd) {
      val secName = readString()
      val secEnd = readEnd()
      sectionReader(secName) = new TastyReader(bytes, currentAddr.index, secEnd.index, currentAddr.index)
      goto(secEnd)
    }
  }
  readNames()
  loadSections()

  def unpickle[R](sec: SectionUnpickler[R]): Option[R] =
    for (reader <- sectionReader.get(sec.name)) yield
      sec.unpickle(reader, nameAtRef)

  private[dotc] def bytes: Array[Byte] = reader.bytes
}
