package dotty.tools.tasty.besteffort

import java.util.UUID

import BestEffortTastyFormat.{MajorVersion, MinorVersion, ExperimentalVersion, bestEffortHeader, header}
import dotty.tools.tasty.{UnpicklerConfig, TastyHeaderUnpickler, TastyReader, UnpickleException, TastyFormat, TastyVersion}

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
  import BestEffortTastyHeaderUnpickler._
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

  private def check(cond: Boolean, msg: => String): Unit = {
    if (!cond) throw new UnpickleException(msg)
  }
}

// Copy pasted from dotty.tools.tasty.TastyHeaderUnpickler
// Since that library has strong compatibility guarantees, we do not want
// to add any more methods just to support an experimental feature
// (like best-effort compilation options).
object BestEffortTastyHeaderUnpickler {

  private def check(cond: Boolean, msg: => String): Unit = {
    if (!cond) throw new UnpickleException(msg)
  }

  private def checkValidVersion(fileMajor: Int, fileMinor: Int, fileExperimental: Int, toolingVersion: String, config: UnpicklerConfig) = {
    val toolMajor: Int = config.majorVersion
    val toolMinor: Int = config.minorVersion
    val toolExperimental: Int = config.experimentalVersion
    val validVersion = TastyFormat.isVersionCompatible(
      fileMajor            = fileMajor,
      fileMinor            = fileMinor,
      fileExperimental     = fileExperimental,
      compilerMajor        = toolMajor,
      compilerMinor        = toolMinor,
      compilerExperimental = toolExperimental
    )
    check(validVersion, {
      // failure means that the TASTy file cannot be read, therefore it is either:
      // - backwards incompatible major, in which case the library should be recompiled by the minimum stable minor
      //   version supported by this compiler
      // - any experimental in an older minor, in which case the library should be recompiled by the stable
      //   compiler in the same minor.
      // - older experimental in the same minor, in which case the compiler is also experimental, and the library
      //   should be recompiled by the current compiler
      // - forward incompatible, in which case the compiler must be upgraded to the same version as the file.
      val fileVersion = TastyVersion(fileMajor, fileMinor, fileExperimental)
      val toolVersion = TastyVersion(toolMajor, toolMinor, toolExperimental)

      val compat = Compatibility.failReason(file = fileVersion, read = toolVersion)

      val what = if (compat < 0) "Backward" else "Forward"
      val signature = signatureString(fileVersion, toolVersion, what, tool = Some(toolingVersion))
      val fix = (
        if (compat < 0) {
          val newCompiler =
            if (compat == Compatibility.BackwardIncompatibleMajor) toolVersion.minStable
            else if (compat == Compatibility.BackwardIncompatibleExperimental) fileVersion.nextStable
            else toolVersion // recompile the experimental library with the current experimental compiler
          recompileFix(newCompiler, config)
        }
        else upgradeFix(fileVersion, config)
      )
      signature + fix + tastyAddendum
    })
  }

  private def signatureString(
      fileVersion: TastyVersion, toolVersion: TastyVersion, what: String, tool: Option[String]) = {
    val optProducedBy = tool.fold("")(t => s", produced by $t")
    s"""$what incompatible TASTy file has version ${fileVersion.show}$optProducedBy,
      |  expected ${toolVersion.validRange}.
      |""".stripMargin
  }

  private def recompileFix(producerVersion: TastyVersion, config: UnpicklerConfig) = {
    val addendum = config.recompileAdditionalInfo
    val newTool = config.upgradedProducerTool(producerVersion)
    s"""  The source of this file should be recompiled by $newTool.$addendum""".stripMargin
  }

  private def upgradeFix(fileVersion: TastyVersion, config: UnpicklerConfig) = {
    val addendum = config.upgradeAdditionalInfo(fileVersion)
    val newTool = config.upgradedReaderTool(fileVersion)
    s"""  To read this ${fileVersion.kind} file, use $newTool.$addendum""".stripMargin
  }

  private def tastyAddendum: String = """
  |  Please refer to the documentation for information on TASTy versioning:
  |  https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html""".stripMargin

  private object Compatibility {
    final val BackwardIncompatibleMajor = -3
    final val BackwardIncompatibleExperimental = -2
    final val ExperimentalRecompile = -1
    final val ExperimentalUpgrade = 1
    final val ForwardIncompatible = 2

    /** Given that file can't be read, extract the reason */
    def failReason(file: TastyVersion, read: TastyVersion): Int =
      if (file.major == read.major && file.minor == read.minor && file.isExperimental && read.isExperimental) {
        if (file.experimental < read.experimental) ExperimentalRecompile // recompile library as compiler is too new
        else ExperimentalUpgrade // they should upgrade compiler as library is too new
      }
      else if (file.major < read.major)
        BackwardIncompatibleMajor // pre 3.0.0
      else if (file.isExperimental && file.major == read.major && file.minor <= read.minor)
        // e.g. 3.4.0 reading 3.4.0-RC1-NIGHTLY, or 3.3.0 reading 3.0.2-RC1-NIGHTLY
        BackwardIncompatibleExperimental
      else ForwardIncompatible
  }
}
