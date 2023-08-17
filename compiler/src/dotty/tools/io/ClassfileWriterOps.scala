package dotty.tools.io

import java.io.{DataOutputStream, IOException, PrintWriter, StringWriter}
import java.nio.file.Files

import dotty.tools.io.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.report
import java.nio.channels.ClosedByInterruptException
import scala.language.unsafeNulls
import scala.annotation.constructorOnly


class ClassfileWriterOps(outputDir: AbstractFile)(using @constructorOnly ictx: Context) {

  type InternalName = String

  // if non-null, classfiles are written to a jar instead of the output directory
  private val jarWriter: JarWriter | Null =
    val localCtx = ictx
    outputDir match {
      case jar: JarArchive =>
        jar.underlyingSource.map { source =>
          if jar.isEmpty then
            new Jar(source.file).jarWriter()
          else inContext(localCtx) {
            // Writing to non-empty JAR might be an undefined behaviour, e.g. in case if other files where
            // created using `AbstractFile.bufferedOutputStream`instead of JarWriter
            report.warning(em"Tried to write to non-empty JAR: $source")
            null
          }
        }.getOrElse(
          inContext(localCtx) {
            report.warning(em"Failed to create JAR writer for $jar")
            null
          }
        )

      case _ => null
  }

  private def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
    if (base.file != null) {
      fastGetFile(base, clsName, suffix)
    } else {
      def ensureDirectory(dir: AbstractFile): AbstractFile =
        if (dir.isDirectory) dir
        else throw new FileConflictException(s"${base.path}/$clsName$suffix: ${dir.path} is not a directory", dir)
      var dir = base
      val pathParts = clsName.split("[./]").toList
      for (part <- pathParts.init) dir = ensureDirectory(dir) subdirectoryNamed part
      ensureDirectory(dir) fileNamed pathParts.last + suffix
    }
  }

  private def fastGetFile(base: AbstractFile, clsName: String, suffix: String) = {
    val index = clsName.lastIndexOf('/')
    val (packageName, simpleName) = if (index > 0) {
      (clsName.substring(0, index), clsName.substring(index + 1))
    } else ("", clsName)
    val directory = base.file.toPath.resolve(packageName)
    new PlainFile(Path(directory.resolve(simpleName + suffix)))
  }

  private def writeBytes(outFile: AbstractFile, bytes: Array[Byte]): Unit = {
    if (outFile.file != null) {
      val outPath = outFile.file.toPath
      try Files.write(outPath, bytes)
      catch {
        case _: java.nio.file.NoSuchFileException =>
          Files.createDirectories(outPath.getParent)
          Files.write(outPath, bytes)
      }
    } else {
      val out = new DataOutputStream(outFile.bufferedOutput)
      try out.write(bytes, 0, bytes.length)
      finally out.close()
    }
  }

  def writeTasty(className: InternalName, bytes: Array[Byte]): Unit =
    writeToJarOrFile(className, bytes, ".tasty")

  private def writeToJarOrFile(className: InternalName, bytes: Array[Byte], suffix: String): AbstractFile | Null = {
    if jarWriter == null then
      val outFolder = outputDir
      val outFile = getFile(outFolder, className, suffix)
      try writeBytes(outFile, bytes)
      catch case ex: ClosedByInterruptException =>
        try outFile.delete() // don't leave an empty or half-written files around after an interrupt
        catch case _: Throwable => ()
        finally throw ex
      outFile
    else
      val path = className + suffix
      val out = jarWriter.newOutputStream(path)
      try out.write(bytes, 0, bytes.length)
      finally out.flush()
      null
  }

  def close(): Unit = {
    if (jarWriter != null) jarWriter.close()
    outputDir match
      case jar: JarArchive => jar.close()
      case _ =>
  }
}


/** Can't output a file due to the state of the file system. */
class FileConflictException(msg: String, val file: AbstractFile) extends IOException(msg)
