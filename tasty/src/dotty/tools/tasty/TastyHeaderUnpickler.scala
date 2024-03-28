package dotty.tools.tasty

import java.util.UUID

import TastyFormat.{MajorVersion, MinorVersion, ExperimentalVersion, header}

/**
 * The Tasty Header consists of four fields:
 * - uuid
 *   - contains a hash of the sections of the TASTy file
 * - majorVersion
 *   - matching the TASTy format version that last broke backwards compatibility
 * - minorVersion
 *   - matching the TASTy format version that last broke forward compatibility
 * - experimentalVersion
 *   - 0 for final compiler version
 *   - positive for between minor versions and forward compatibility
 *     is broken since the previous stable version.
 * - toolingVersion
 *   - arbitrary string representing the tooling that produced the TASTy
 */
sealed abstract case class TastyHeader(
  uuid: UUID,
  majorVersion: Int,
  minorVersion: Int,
  experimentalVersion: Int,
  toolingVersion: String
)

trait UnpicklerConfig {
  /** The TASTy major version that this reader supports */
  def majorVersion: Int
  /** The TASTy minor version that this reader supports */
  def minorVersion: Int
  /** The TASTy experimental version that this reader supports */
  def experimentalVersion: Int
  /** The description of the upgraded tool that can read the given TASTy version */
  def upgradedReaderTool(version: TastyVersion): String
  /** The description of the upgraded tool that can produce the given TASTy version */
  def upgradedProducerTool(version: TastyVersion): String
  /** Additional information to help a user fix the outdated TASTy problem */
  def recompileAdditionalInfo: String
  /** Additional information to help a user fix the more recent TASTy problem */
  def upgradeAdditionalInfo(fileVersion: TastyVersion): String
}

object UnpicklerConfig {

  /** A config where its major, minor and experimental versions are fixed to those in TastyFormat */
  trait DefaultTastyVersion extends UnpicklerConfig {
    override final def majorVersion: Int = MajorVersion
    override final def minorVersion: Int = MinorVersion
    override final def experimentalVersion: Int = ExperimentalVersion
  }

  trait Generic extends UnpicklerConfig {
    final def upgradedProducerTool(version: TastyVersion): String =
      "a later version"

    final def upgradedReaderTool(version: TastyVersion): String =
      if (version.isExperimental) s"the version of this tool compatible with TASTy ${version.show}"
      else s"a newer version of this tool compatible with TASTy ${version.show}"

    final def recompileAdditionalInfo: String = """
      |  Usually this means that the classpath entry of this file should be updated.""".stripMargin

    final def upgradeAdditionalInfo(fileVersion: TastyVersion): String =
      if (fileVersion.isExperimental && experimentalVersion == 0) {
        """
          |  Note that this tool does not support reading experimental TASTy.""".stripMargin
      }
      else ""
  }

  /** A config for the TASTy reader of a generic tool */
  val generic: UnpicklerConfig = new UnpicklerConfig with Generic with DefaultTastyVersion {}
}

class TastyHeaderUnpickler(config: UnpicklerConfig, reader: TastyReader) {
  import TastyHeaderUnpickler._
  import reader._

  def this(config: UnpicklerConfig, bytes: Array[Byte]) = this(config, new TastyReader(bytes))
  def this(reader: TastyReader) = this(UnpicklerConfig.generic, reader)
  def this(bytes: Array[Byte]) = this(new TastyReader(bytes))

  private val toolMajor: Int = config.majorVersion
  private val toolMinor: Int = config.minorVersion
  private val toolExperimental: Int = config.experimentalVersion

  /** reads and verifies the TASTy version, extracting the UUID */
  def readHeader(): UUID =
    readFullHeader().uuid

  /** reads and verifies the TASTy version, extracting the whole header */
  def readFullHeader(): TastyHeader = {

    for (i <- 0 until header.length)
      check(readByte() == header(i), "not a TASTy file")
    val fileMajor = readNat()
    if (fileMajor <= 27) { // old behavior before `tasty-core` 3.0.0-M4
      val fileMinor = readNat()
      val fileVersion = TastyVersion(fileMajor, fileMinor, 0)
      val toolVersion = TastyVersion(toolMajor, toolMinor, toolExperimental)
      val signature = signatureString(fileVersion, toolVersion, what = "Backward", tool = None)
      val fix = recompileFix(toolVersion.minStable, config)
      throw new UnpickleException(signature + fix + tastyAddendum)
    }
    else {
      val fileMinor = readNat()
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
      new TastyHeader(uuid, fileMajor, fileMinor, fileExperimental, toolingVersion) {}
    }
  }

  def isAtEnd: Boolean = reader.isAtEnd
}

object TastyHeaderUnpickler {

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
