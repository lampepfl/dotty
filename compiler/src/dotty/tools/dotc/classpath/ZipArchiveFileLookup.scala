/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import scala.language.unsafeNulls

import java.io.File
import java.net.URL

import dotty.tools.io.{ AbstractFile, FileZipArchive }
import FileUtils.*
import dotty.tools.io.{EfficientClassPath, ClassRepresentation}

/**
 * A trait allowing to look for classpath entries of given type in zip and jar files.
 * It provides common logic for classes handling class and source files.
 * It's aware of things like e.g. META-INF directory which is correctly skipped.
 */
trait ZipArchiveFileLookup[FileEntryType <: ClassRepresentation] extends EfficientClassPath {
  val zipFile: File
  def release: Option[String]

  assert(zipFile != null, "Zip file in ZipArchiveFileLookup cannot be null")

  override def asURLs: Seq[URL] = Seq(zipFile.toURI.toURL)
  override def asClassPathStrings: Seq[String] = Seq(zipFile.getPath)

  private val archive = new FileZipArchive(zipFile.toPath, release)

  override private[dotty] def packages(inPackage: PackageName): Seq[PackageEntry] = {
    for {
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if entry.isPackage
    }
    yield PackageEntryImpl(inPackage.entryName(entry.name))
  }

  protected def files(inPackage: PackageName): Seq[FileEntryType] =
    for {
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if isRequiredFileType(entry)
    }
    yield createFileEntry(entry)

  protected def file(inPackage: PackageName, name: String): Option[FileEntryType] =
    for {
      dirEntry <- findDirEntry(inPackage)
      entry <- Option(dirEntry.lookupName(name, directory = false))
      if isRequiredFileType(entry)
    }
    yield createFileEntry(entry)

  override def hasPackage(pkg: PackageName) = findDirEntry(pkg).isDefined
  def list(inPackage: PackageName, onPackageEntry: PackageEntry => Unit, onClassesAndSources: ClassRepresentation => Unit): Unit =
    findDirEntry(inPackage) match {
      case Some(dirEntry) =>
        for (entry <- dirEntry.iterator) {
          if (entry.isPackage)
            onPackageEntry(PackageEntryImpl(inPackage.entryName(entry.name)))
          else if (isRequiredFileType(entry))
            onClassesAndSources(createFileEntry(entry))
        }
      case None =>
    }

  private def findDirEntry(pkg: PackageName): Option[archive.DirEntry] =
    archive.allDirs.get(pkg.dirPathTrailingSlashJar)

  protected def createFileEntry(file: FileZipArchive#Entry): FileEntryType
  protected def isRequiredFileType(file: AbstractFile): Boolean
}
