package dotty.tools.io

import scala.language.unsafeNulls

import java.nio.file.{FileSystemAlreadyExistsException, FileSystems}

import scala.jdk.CollectionConverters.*

/**
 * This class implements an [[AbstractFile]] backed by a jar
 * that be can used as the compiler's output directory.
 */
class JarArchive private (root: Directory) extends PlainDirectory(root) {
  def close(): Unit = jpath.getFileSystem().close()
  override def exists: Boolean = jpath.getFileSystem().isOpen() && super.exists
  def allFileNames(): Iterator[String] =
    java.nio.file.Files.walk(jpath).iterator().asScala.map(_.toString)
}

object JarArchive {
  /** Create a new jar file. Overwrite if file already exists */
  def create(path: Path): JarArchive = {
    require(path.ext.isJar)
    path.delete()
    open(path, create = true)
  }

  /** Create a jar file. */
  def open(path: Path, create: Boolean = false): JarArchive = {
    require(path.ext.isJar)

    // creating a new zip file system by using the JAR URL syntax:
    // https://docs.oracle.com/javase/7/docs/technotes/guides/io/fsp/zipfilesystemprovider.html
    val env = Map("create" -> create.toString).asJava
    val uri = java.net.URI.create("jar:" + path.toAbsolute.toURI.toString)
    val fs = {
      try FileSystems.newFileSystem(uri, env)
      catch {
        case _: FileSystemAlreadyExistsException => FileSystems.getFileSystem(uri)
      }
    }
    val root = fs.getRootDirectories().iterator.next()
    new JarArchive(Directory(root))
  }
}
