/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package dotty.tools.io

import scala.language.unsafeNulls

import java.io.RandomAccessFile
import java.nio.file.*
import java.net.{URI, URL}
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.io.IOException
import scala.jdk.CollectionConverters.*
import scala.util.Random.alphanumeric

/** An abstraction for filesystem paths.  The differences between
 *  Path, File, and Directory are primarily to communicate intent.
 *  Since the filesystem can change at any time, there is no way to
 *  reliably associate Files only with files and so on.  Any Path
 *  can be converted to a File or Directory (and thus gain access to
 *  the additional entity specific methods) by calling toFile or
 *  toDirectory, which has no effect on the filesystem.
 *
 *  Also available are createFile and createDirectory, which attempt
 *  to create the path in question.
 *
 *  @author  Paul Phillips
 *  @since   2.8
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object Path {
  def isExtensionJarOrZip(jpath: JPath): Boolean = isExtensionJarOrZip(jpath.getFileName.toString)
  def isExtensionJarOrZip(name: String): Boolean = fileExtension(name).isJarOrZip
  def fileExtension(name: String): FileExtension = {
    val i = name.lastIndexOf('.')
    if (i < 0) FileExtension.Empty
    else FileExtension.from(name.substring(i + 1))
  }
  @deprecated("use fileExtension instead.")
  def extension(name: String): String = fileExtension(name).toLowerCase

  /** strip anything after and including trailing the extension */
  def fileName(name: String): String = {
    val i = name.lastIndexOf('.')
    if (i < 0) name
    else name.substring(0, i).nn
  }

  def onlyDirs(xs: Iterator[Path]): Iterator[Directory] = xs.filter(_.isDirectory).map(_.toDirectory)
  def onlyDirs(xs: List[Path]): List[Directory] = xs.filter(_.isDirectory).map(_.toDirectory)
  def onlyFiles(xs: Iterator[Path]): Iterator[File] = xs.filter(_.isFile).map(_.toFile)

  def roots: List[Path] = FileSystems.getDefault.getRootDirectories.iterator().asScala.map(Path.apply).toList

  def apply(path: String): Path = apply(new java.io.File(path).toPath)
  def apply(jpath: JPath): Path = try {
    if (Files.isRegularFile(jpath)) new File(jpath)
    else if (Files.isDirectory(jpath)) new Directory(jpath)
    else new Path(jpath)
  } catch { case ex: SecurityException => new Path(jpath) }

  /** Avoiding any shell/path issues by only using alphanumerics. */
  private[io] def randomPrefix: String = alphanumeric take 6 mkString ""
  private[io] def fail(msg: String): Nothing = throw FileOperationException(msg)
}
import Path.*

/** The Path constructor is private so we can enforce some
 *  semantics regarding how a Path might relate to the world.
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class Path private[io] (val jpath: JPath) {
  val separator: Char = java.io.File.separatorChar
  val separatorStr: String = java.io.File.separator

  // conversions
  def toFile: File = new File(jpath)
  def toDirectory: Directory = new Directory(jpath)
  def toAbsolute: Path = if (isAbsolute) this else new Path(jpath.toAbsolutePath)
  def toCanonical: Path = normalize.toAbsolute
  def toURI: URI = jpath.toUri()
  def toURL: URL = toURI.toURL()

  /** If this path is absolute, returns it: otherwise, returns an absolute
   *  path made up of root / this.
   */
  def toAbsoluteWithRoot(root: Path): Path = if (isAbsolute) this else root.toAbsolute / this

  /** Creates a new Path with the specified path appended.  Assumes
   *  the type of the new component implies the type of the result.
   */
  def /(child: String): Path = new Path(jpath.resolve(child))
  def /(child: Path): Path = resolve(child)
  def /(child: Directory): Directory = /(child: Path).toDirectory
  def /(child: File): File = /(child: Path).toFile

  /** If this path is a directory, recursively iterate over its contents.
   *  The supplied condition is a filter which is applied to each element,
   *  with that branch of the tree being closed off if it is false.
   *  So for example if the condition is false for some subdirectory, nothing
   *  under that directory will be in the Iterator. If it's true, all files for
   *  which the condition holds and are directly in that subdirectory are in the
   *  Iterator, and all sub-subdirectories are recursively evaluated
   */
  def walkFilter(cond: Path => Boolean): Iterator[Path] =
    if (isFile) toFile walkFilter cond
    else if (isDirectory) toDirectory.walkFilter(cond)
    else Iterator.empty

  /** Equivalent to walkFilter(_ => true).
   */
  def walk: Iterator[Path] = walkFilter(_ => true)

  // identity
  def name: String = jpath.getFileName() match {
    case null => ""
    case name => name.toString
  }
  def path: String = jpath.toString
  def normalize: Path = new Path(jpath.normalize)

  def resolve(other: Path): Path = new Path(jpath.resolve(other.jpath))
  def relativize(other: Path): Path = new Path(jpath.relativize(other.jpath))

  def segments: List[String] = (path split separator).toList filterNot (_.length == 0)

  /**
   * @return The path of the parent directory, or root if path is already root
   */
  def parent: Directory = {
    // We don't call JPath#normalize here because it may result in resolving
    // to a different path than intended, such as when the given path contains
    // a `..` component and the preceding name is a symbolic link.
    // https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html#normalize--
    //
    // Paths ending with `..` or `.` are handled specially here as
    // JPath#getParent wants to simply strip away that last element.
    // For the `..` case, we should take care not to fall into the above trap
    // as the preceding name may be a symbolic link. This leaves little choice
    // (since we don't want to access the file system) but to return the given
    // path with `..` appended. By contrast, a `.` as the final path element
    // is always redundant and should be removed before computing the parent.
    if path.isEmpty then
      Directory("..")
    else if jpath.endsWith("..") then
      (this / "..").toDirectory
    else if jpath.endsWith(".") then
      jpath.getParent match   // strip the trailing `.` element
        case null => Directory("..")
        case p    => new Path(p).parent
    else jpath.getParent match
      case null =>
        if isAbsolute then toDirectory  // it should be a root
        else Directory(".")
      case x =>
        Directory(x)
  }
  def parents: List[Directory] = {
    val p = parent
    if (p isSame this) Nil else p :: p.parents
  }

  def ext: FileExtension = Path.fileExtension(name)

  // if name ends with an extension (e.g. "foo.jpg") returns the extension ("jpg"), otherwise ""
  @deprecated("use ext instead.")
  def extension: String = ext.toLowerCase

  // compares against extensions in a CASE INSENSITIVE way.
  @deprecated("consider using queries on ext instead.")
  def hasExtension(ext: String, exts: String*): Boolean = {
    val lower = ext.toLowerCase
    lower.equalsIgnoreCase(ext) || exts.exists(lower.equalsIgnoreCase)
  }
  // returns the filename without the extension.
  def stripExtension: String = Path.fileName(name)
  // returns the Path with the extension.
  def addExtension(ext: String): Path = new Path(jpath.resolveSibling(name + ext))

  // changes the existing extension out for a new one, or adds it
  // if the current path has none.
  def changeExtension(ext: FileExtension): Path =
    changeExtension(ext.toLowerCase)

  // changes the existing extension out for a new one, or adds it
  // if the current path has none.
  def changeExtension(ext: String): Path =
    val name0 = name
    val dropExtension = Path.fileName(name0)
    if dropExtension eq name0 then addExtension(ext)
    else new Path(jpath.resolveSibling(dropExtension + "." + ext))

  // conditionally execute
  def ifFile[T](f: File => T): Option[T] = if (isFile) Some(f(toFile)) else None
  def ifDirectory[T](f: Directory => T): Option[T] = if (isDirectory) Some(f(toDirectory)) else None

  // Boolean tests
  def canRead: Boolean = Files.isReadable(jpath)
  def canWrite: Boolean = Files.isWritable(jpath)
  def exists: Boolean = try Files.exists(jpath)  catch { case ex: SecurityException => false }
  def isFile: Boolean = try Files.isRegularFile(jpath)  catch { case ex: SecurityException => false }
  def isDirectory: Boolean =
    try Files.isDirectory(jpath)
    catch { case ex: SecurityException => jpath.toString == "." }
  def isAbsolute: Boolean = jpath.isAbsolute()
  def isEmpty: Boolean = path.length == 0

  // Information
  def lastModified: FileTime = Files.getLastModifiedTime(jpath)
  def length: Long = Files.size(jpath)

  // Boolean path comparisons
  def endsWith(other: Path): Boolean = segments endsWith other.segments
  def isSame(other: Path): Boolean = toCanonical == other.toCanonical
  def isFresher(other: Path): Boolean = lastModified.compareTo(other.lastModified) > 0

  // creations
  def createDirectory(force: Boolean = true, failIfExists: Boolean = false): Directory = {
    val res = tryCreate(if (force) Files.createDirectories(jpath) else Files.createDirectory(jpath))
    if (!res && failIfExists && exists) fail("Directory '%s' already exists." format name)
    else if (isDirectory) toDirectory
    else new Directory(jpath)
  }
  def createFile(failIfExists: Boolean = false): File = {
    val res = tryCreate(Files.createFile(jpath))
    Files.createFile(jpath)
    if (!res && failIfExists && exists) fail("File '%s' already exists." format name)
    else if (isFile) toFile
    else new File(jpath)
  }

  private def tryCreate(create: => JPath): Boolean =
    try { create; true } catch { case _: FileAlreadyExistsException => false }

  // deletions
  def delete(): Unit =
    try { Files.deleteIfExists(jpath) } catch { case _: DirectoryNotEmptyException => }

  /** Deletes the path recursively. Returns false on failure.
   *  Use with caution!
   */
  def deleteRecursively(): Boolean = {
    if (!exists) false
    else {
      Files.walkFileTree(jpath, new SimpleFileVisitor[JPath]() {
        override def visitFile(file: JPath, attrs: BasicFileAttributes) = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: JPath, exc: IOException) = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
      true
    }
  }

  def truncate(): Boolean =
    isFile && {
      val raf = new RandomAccessFile(jpath.toFile, "rw")
      raf setLength 0
      raf.close()
      length == 0
    }

  override def toString(): String = path
  override def equals(other: Any): Boolean = other match {
    case x: Path  => path == x.path
    case _        => false
  }
  override def hashCode(): Int = path.hashCode()
}
