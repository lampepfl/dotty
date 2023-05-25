package dotty.tools.tasty.besteffort

import java.util.UUID

import BestEffortTastyFormat.{MajorVersion, MinorVersion, ExperimentalVersion, bestEffortHeader, header}
import dotty.tools.tasty.{UnpicklerConfig, TastyHeaderUnpickler, TastyReader, UnpickleException, TastyFormat}

/**
 * The Best Effort Tasty Header consists of six fields:
 * - uuid
 *   - contains a hash of the sections of the Best Effort TASTy file
 * - majorVersion
 *   - matching the TASTy format version that last broke backwards compatibility
 * - minorVersion
 *   - matching the TASTy format version that last broke forward compatibility
 * - patchVersion
 *   - specyfing the best effort TASTy version. Currently unused, kept as a reserved space.
 *     Empty if it was serialized as a regular TASTy file with reagular tasty header.
 * - experimentalVersion
 *   - 0 for final compiler version
 *   - positive for between minor versions and forward compatibility
 *     is broken since the previous stable version.
 * - toolingVersion
 *   - arbitrary string representing the tooling that produced the Best Effort TASTy
 */
sealed abstract case class BestEffortTastyHeader(
  uuid: UUID,
  majorVersion: Int,
  minorVersion: Int,
  patchVersion: Option[Int],
  experimentalVersion: Int,
  toolingVersion: String
)

class BestEffortTastyHeaderUnpickler(config: UnpicklerConfig, reader: TastyReader) {
  import TastyHeaderUnpickler._
  import reader._

  def this(reader: TastyReader) = this(UnpicklerConfig.generic, reader)
  def this(bytes: Array[Byte]) = this(new TastyReader(bytes))

  def readHeader(): UUID =
    readFullHeader().uuid

  def readFullHeader(): BestEffortTastyHeader = {
    val hasBestEffortHeader = {
      val readHeader = (for (i <- 0 until header.length) yield readByte()).toArray

      if (readHeader.sameElements(header)) false
      else if (readHeader.sameElements(bestEffortHeader)) true
      else throw new UnpickleException("not a TASTy or Best Effort TASTy file")
    }

    val fileMajor = readNat()
    val fileMinor = readNat()
    val filePatch =
      if hasBestEffortHeader then Some(readNat())
      else None
    val fileExperimental = readNat()
    val toolingVersion = {
      val length = readNat()
      val start = currentAddr
      val end = start + length
      goto(end)
      new String(bytes, start.index, length)
    }

    checkValidVersion(fileMajor, fileMinor, fileExperimental, toolingVersion, config)

    val uuid = new UUID(readUncompressedLong(), readUncompressedLong())
    new BestEffortTastyHeader(uuid, fileMajor, fileMinor, filePatch, fileExperimental, toolingVersion) {}
  }

  private[tasty] def check(cond: Boolean, msg: => String): Unit = {
    if (!cond) throw new UnpickleException(msg)
  }
}
