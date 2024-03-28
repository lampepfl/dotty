package dotty.tools.dotc.classpath

import scala.language.unsafeNulls

import dotty.tools.io.{ClassPath, ClassRepresentation}
import dotty.tools.io.{AbstractFile, VirtualDirectory}
import FileUtils.*
import java.net.{URI, URL}

case class VirtualDirectoryClassPath(dir: VirtualDirectory) extends ClassPath with DirectoryLookup[BinaryFileEntry] with NoSourcePaths {
  type F = AbstractFile

  // From AbstractFileClassLoader
  private final def lookupPath(base: AbstractFile)(pathParts: Seq[String], directory: Boolean): AbstractFile = {
    var file: AbstractFile = base
    val dirParts = pathParts.init.iterator
    while (dirParts.hasNext) {
      val dirPart = dirParts.next
      file = file.lookupName(dirPart, directory = true)
      if (file == null)
        return null
    }
    file.lookupName(pathParts.last, directory = directory)
  }

  protected def emptyFiles: Array[AbstractFile] = Array.empty
  protected def getSubDir(packageDirName: String): Option[AbstractFile] =
    Option(lookupPath(dir)(packageDirName.split(java.io.File.separator).toIndexedSeq, directory = true))
  protected def listChildren(dir: AbstractFile, filter: Option[AbstractFile => Boolean] = None): Array[F] = filter match {
    case Some(f) => dir.iterator.filter(f).toArray
    case _ => dir.toArray
  }
  def getName(f: AbstractFile): String = f.name
  def toAbstractFile(f: AbstractFile): AbstractFile = f
  def isPackage(f: AbstractFile): Boolean = f.isPackage

  // mimic the behavior of the old nsc.util.DirectoryClassPath
  def asURLs: Seq[URL] = Seq(new URI(dir.name).toURL)
  def asClassPathStrings: Seq[String] = Seq(dir.path)

  def findClassFile(className: String): Option[AbstractFile] = {
    val pathSeq = FileUtils.dirPath(className).split(java.io.File.separator)
    val parentDir = lookupPath(dir)(pathSeq.init.toSeq, directory = true)
    if parentDir == null then None
    else
      Option(lookupPath(parentDir)(pathSeq.last + ".class" :: Nil, directory = false))
  }

  private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry] = files(inPackage)

  protected def createFileEntry(file: AbstractFile): BinaryFileEntry = BinaryFileEntry(file)

  protected def isMatchingFile(f: AbstractFile): Boolean =
    f.isTasty || (f.isClass && !f.hasSiblingTasty)
}
