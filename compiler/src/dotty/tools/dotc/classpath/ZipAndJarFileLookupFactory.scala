/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc
package classpath

import scala.language.unsafeNulls

import java.io.File
import java.net.URL
import java.nio.file.Files
import java.nio.file.attribute.{BasicFileAttributes, FileTime}

import scala.annotation.tailrec
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation, FileZipArchive, ManifestResources}
import dotty.tools.dotc.core.Contexts.*
import FileUtils.*

/**
 * A trait providing an optional cache for classpath entries obtained from zip and jar files.
 * It allows us to e.g. reduce significantly memory used by PresentationCompilers in Scala IDE
 * when there are a lot of projects having a lot of common dependencies.
 */
sealed trait ZipAndJarFileLookupFactory {
  private val cache = new FileBasedCache[ClassPath]

  def create(zipFile: AbstractFile)(using Context): ClassPath =
    val release = Option(ctx.settings.javaOutputVersion.value).filter(_.nonEmpty)
    if (ctx.settings.YdisableFlatCpCaching.value || zipFile.file == null) createForZipFile(zipFile, release)
    else createUsingCache(zipFile, release)

  protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath

  private def createUsingCache(zipFile: AbstractFile, release: Option[String]): ClassPath =
    cache.getOrCreate(zipFile.file.toPath, () => createForZipFile(zipFile, release))
}

/**
 * Manages creation of classpath for class files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarClassPathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveClassPath(zipFile: File, override val release: Option[String])
    extends ZipArchiveFileLookup[BinaryFileEntry]
    with NoSourcePaths {

    override def findClassFile(className: String): Option[AbstractFile] =
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(PackageName(pkg), simpleClassName + ".class").map(_.file)

    override private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): BinaryFileEntry = BinaryFileEntry(file)

    override protected def isRequiredFileType(file: AbstractFile): Boolean =
      file.isTasty || (file.isClass && !file.hasSiblingTasty)
  }

  /**
   * This type of classpath is closely related to the support for JSR-223.
   * Its usage can be observed e.g. when running:
   * jrunscript -classpath scala-compiler.jar;scala-reflect.jar;scala-library.jar -l scala
   * with a particularly prepared scala-library.jar. It should have all classes listed in the manifest like e.g. this entry:
   * Name: scala/Function2$mcFJD$sp.class
   */
  private case class ManifestResourcesClassPath(file: ManifestResources) extends ClassPath with NoSourcePaths {
    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      classes(PackageName(pkg)).find(_.name == simpleClassName).map(_.file)
    }

    override def asClassPathStrings: Seq[String] = Seq(file.path)

    override def asURLs: Seq[URL] = file.toURLs()

    import ManifestResourcesClassPath.PackageFileInfo
    import ManifestResourcesClassPath.PackageInfo

    /**
     * A cache mapping package name to abstract file for package directory and subpackages of given package.
     *
     * ManifestResources can iterate through the collections of entries from e.g. remote jar file.
     * We can't just specify the path to the concrete directory etc. so we can't just 'jump' into
     * given package, when it's needed. On the other hand we can iterate over entries to get
     * AbstractFiles, iterate over entries of these files etc.
     *
     * Instead of traversing a tree of AbstractFiles once and caching all entries or traversing each time,
     * when we need subpackages of a given package or its classes, we traverse once and cache only packages.
     * Classes for given package can be then easily loaded when they are needed.
     */
    private lazy val cachedPackages: util.HashMap[String, PackageFileInfo] = {
      val packages = util.HashMap[String, PackageFileInfo]()

      def getSubpackages(dir: AbstractFile): List[AbstractFile] =
        (for (file <- dir if file.isPackage) yield file).toList

      @tailrec
      def traverse(packagePrefix: String,
                   filesForPrefix: List[AbstractFile],
                   subpackagesQueue: collection.mutable.Queue[PackageInfo]): Unit = filesForPrefix match {
        case pkgFile :: remainingFiles =>
          val subpackages = getSubpackages(pkgFile)
          val fullPkgName = packagePrefix + pkgFile.name
          packages(fullPkgName) = PackageFileInfo(pkgFile, subpackages)
          val newPackagePrefix = fullPkgName + "."
          subpackagesQueue.enqueue(PackageInfo(newPackagePrefix, subpackages))
          traverse(packagePrefix, remainingFiles, subpackagesQueue)
        case Nil if subpackagesQueue.nonEmpty =>
          val PackageInfo(packagePrefix, filesForPrefix) = subpackagesQueue.dequeue()
          traverse(packagePrefix, filesForPrefix, subpackagesQueue)
        case _ =>
      }

      val subpackages = getSubpackages(file)
      packages(ClassPath.RootPackage) = PackageFileInfo(file, subpackages)
      traverse(ClassPath.RootPackage, subpackages, collection.mutable.Queue())
      packages
    }

    override private[dotty] def packages(inPackage: PackageName): Seq[PackageEntry] = cachedPackages.get(inPackage.dottedString) match {
      case None => Seq.empty
      case Some(PackageFileInfo(_, subpackages)) =>
        subpackages.map(packageFile => PackageEntryImpl(inPackage.entryName(packageFile.name)))
    }

    override private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry] = cachedPackages.get(inPackage.dottedString) match {
      case None => Seq.empty
      case Some(PackageFileInfo(pkg, _)) =>
        (for (file <- pkg if file.isClass) yield ClassFileEntry(file)).toSeq
    }

    override private[dotty] def hasPackage(pkg: PackageName) = cachedPackages.contains(pkg.dottedString)
    override private[dotty] def list(inPackage: PackageName): ClassPathEntries = ClassPathEntries(packages(inPackage), classes(inPackage))
  }

  private object ManifestResourcesClassPath {
    case class PackageFileInfo(packageFile: AbstractFile, subpackages: Seq[AbstractFile])
    case class PackageInfo(packageName: String, subpackages: List[AbstractFile])
  }

  override protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath =
    if (zipFile.file == null) createWithoutUnderlyingFile(zipFile)
    else ZipArchiveClassPath(zipFile.file, release)

  private def createWithoutUnderlyingFile(zipFile: AbstractFile) = zipFile match {
    case manifestRes: ManifestResources =>
      ManifestResourcesClassPath(manifestRes)
    case _ =>
      val errorMsg = s"Abstract files which don't have an underlying file and are not ManifestResources are not supported. There was $zipFile"
      throw new IllegalArgumentException(errorMsg)
  }
}

/**
 * Manages creation of classpath for source files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarSourcePathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveSourcePath(zipFile: File)
    extends ZipArchiveFileLookup[SourceFileEntry]
    with NoClassPaths {

    def release: Option[String] = None

    override def asSourcePathString: String = asClassPathString

    override private[dotty] def sources(inPackage: PackageName): Seq[SourceFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): SourceFileEntry = SourceFileEntry(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isScalaOrJavaSource
  }

  override protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath = ZipArchiveSourcePath(zipFile.file)
}

final class FileBasedCache[T] {
  private case class Stamp(lastModified: FileTime, fileKey: Object)
  private val cache = collection.mutable.Map.empty[java.nio.file.Path, (Stamp, T)]

  def getOrCreate(path: java.nio.file.Path, create: () => T): T = cache.synchronized {
    val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
    val lastModified = attrs.lastModifiedTime()
    // only null on some platforms, but that's okay, we just use the last modified timestamp as our stamp
    val fileKey = attrs.fileKey()
    val stamp = Stamp(lastModified, fileKey)
    cache.get(path) match {
      case Some((cachedStamp, cached)) if cachedStamp == stamp => cached
      case _ =>
        val value = create()
        cache.put(path, (stamp, value))
        value
    }
  }

  def clear(): Unit = cache.synchronized {
    // TODO support closing
    // cache.valuesIterator.foreach(_.close())
    cache.clear()
  }
}
