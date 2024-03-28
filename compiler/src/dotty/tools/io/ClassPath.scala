/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package dotty.tools
package io

import scala.language.unsafeNulls

import java.net.{MalformedURLException, URI, URISyntaxException, URL}
import java.util.regex.PatternSyntaxException

import File.pathSeparator
import Jar.isJarOrZip

import dotc.classpath.{ PackageEntry, ClassPathEntries, PackageName }

/**
  * A representation of the compiler's class- or sourcepath.
  */
trait ClassPath {
  import dotty.tools.dotc.classpath.*
  def asURLs: Seq[URL]

  final def hasPackage(pkg: String): Boolean = hasPackage(PackageName(pkg))
  final def packages(inPackage: String): Seq[PackageEntry] = packages(PackageName(inPackage))
  final def classes(inPackage: String): Seq[BinaryFileEntry] = classes(PackageName(inPackage))
  final def sources(inPackage: String): Seq[SourceFileEntry] = sources(PackageName(inPackage))
  final def list(inPackage: String): ClassPathEntries = list(PackageName(inPackage))

  /*
   * These methods are mostly used in the ClassPath implementation to implement the `list` and
   * `findX` methods below.
   *
   * However, there are some other uses in the compiler, to implement `invalidateClassPathEntries`,
   * which is used by the repl's `:require` (and maybe the spark repl, https://github.com/scala/scala/pull/4051).
   * Using these methods directly is more efficient than calling `list`.
   *
   * The `inPackage` contains a full package name, e.g. "" or "scala.collection".
   */

  private[dotty] def hasPackage(pkg: PackageName): Boolean
  private[dotty] def packages(inPackage: PackageName): Seq[PackageEntry]
  private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry]
  private[dotty] def sources(inPackage: PackageName): Seq[SourceFileEntry]

  /**
    * Returns packages and classes (source or classfile) that are members of `inPackage` (not
    * recursively). The `inPackage` contains a full package name, e.g., "scala.collection".
    *
    * This is the main method uses to find classes, see class `PackageLoader`. The
    * `rootMirror.rootLoader` is created with `inPackage = ""`.
    */
  private[dotty] def list(inPackage: PackageName): ClassPathEntries

  /**
   * Returns *only* the classfile for an external name, e.g., "java.lang.String". This method does not
   * return source files, tasty files,.
   *
   * This method is used by the classfile parser. When parsing a Java class, its own inner classes
   * are entered with a `ClassfileLoader` that parses the classfile returned by this method.
   * It is also used in the backend, by the inliner, to obtain the bytecode when inlining from the
   * classpath. It's also used by scalap.
   */
  def findClassFile(className: String): Option[AbstractFile]

  def asClassPathStrings: Seq[String]

  /** The whole classpath in the form of one String.
    */
  def asClassPathString: String = ClassPath.join(asClassPathStrings*)
  // for compatibility purposes
  @deprecated("use asClassPathString instead of this one", "2.11.5")
  def asClasspathString: String = asClassPathString

  /** The whole sourcepath in the form of one String.
    */
  def asSourcePathString: String
}

trait EfficientClassPath extends ClassPath {
  def list(inPackage: PackageName, onPackageEntry: PackageEntry => Unit, onClassesAndSources: ClassRepresentation => Unit): Unit

  override def list(inPackage: PackageName): ClassPathEntries = {
    val packageBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
    val classRepBuf = collection.mutable.ArrayBuffer.empty[ClassRepresentation]
    list(inPackage, packageBuf += _, classRepBuf += _)
    if (packageBuf.isEmpty && classRepBuf.isEmpty) ClassPathEntries.empty
    else ClassPathEntries(packageBuf, classRepBuf)
  }
}

trait EfficientClassPathCallBack {
  def packageEntry(entry: PackageEntry): Unit
  def classesAndSources(entry: ClassRepresentation): Unit
}

object ClassPath {
  val RootPackage: String = ""

  /** Expand single path entry */
  private def expandS(pattern: String): List[String] = {
    val wildSuffix = File.separator + "*"

    /* Get all subdirectories, jars, zips out of a directory. */
    def lsDir(dir: Directory, filt: String => Boolean = _ => true) =
      dir.list.filter(x => filt(x.name) && (x.isDirectory || isJarOrZip(x))).map(_.path).toList

    if (pattern == "*") lsDir(Directory("."))
    // On Windows the JDK supports forward slash or backslash in classpath entries
    else if (pattern.endsWith(wildSuffix) || pattern.endsWith("/*")) lsDir(Directory(pattern dropRight 2))
    else if (pattern.contains('*')) {
      try {
        val regexp = ("^" + pattern.replace("""\*""", """.*""") + "$").r
        lsDir(Directory(pattern).parent, regexp.findFirstIn(_).isDefined)
      }
      catch { case _: PatternSyntaxException => List(pattern) }
    }
    else List(pattern)
  }

  /** Split classpath using platform-dependent path separator */
  def split(path: String): List[String] = path.split(pathSeparator).toList.filterNot(_ == "").distinct

  /** Join classpath using platform-dependent path separator */
  def join(paths: String*): String  = paths.filterNot(_ == "").mkString(pathSeparator)

  /** Split the classpath, apply a transformation function, and reassemble it. */
  def map(cp: String, f: String => String): String = join(split(cp).map(f)*)

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) split(path).flatMap(expandS)
    else split(path)

  /** Expand dir out to contents, a la extdir */
  def expandDir(extdir: String): List[String] = {
    AbstractFile getDirectory extdir match {
      case null => Nil
      case dir  => dir.filter(_.isClassContainer).map(x => new java.io.File(dir.file, x.name).getPath).toList
    }
  }

  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  def expandManifestPath(jarPath: String): List[URL] = {
    val file = File(jarPath)
    if (!file.isFile) return Nil

    val baseDir = file.parent
    new Jar(file).classPathElements map (elem =>
      specToURL(elem) getOrElse (baseDir / elem).toURL
    )
  }

  def specToURL(spec: String): Option[URL] =
    try Some(new URI(spec).toURL)
    catch case _: MalformedURLException | _: URISyntaxException => None

  def manifests: List[java.net.URL] = {
    import scala.jdk.CollectionConverters.EnumerationHasAsScala
    val resources = Thread.currentThread().getContextClassLoader().getResources("META-INF/MANIFEST.MF")
    resources.asScala.filter(_.getProtocol == "jar").toList
  }

  @deprecated("shim for sbt's compiler interface", since = "2.12.0")
  sealed abstract class ClassPathContext

  @deprecated("shim for sbt's compiler interface", since = "2.12.0")
  sealed abstract class JavaContext
}

trait ClassRepresentation {
  def fileName: String
  def name: String
  def binary: Option[AbstractFile]
  def source: Option[AbstractFile]

  /** returns the length of `name` by stripping the extension of `fileName`
   *
   *  Used to avoid creating String instance of `name`.
   */
  final def nameLength: Int = {
    val ix = fileName.lastIndexOf('.')
    if (ix < 0) fileName.length else ix
  }
}

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class DirectoryClassPath

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class MergedClassPath

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class JavaClassPath
