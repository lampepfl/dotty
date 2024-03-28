/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import dotty.tools.dotc.classpath.FileUtils.isTasty
import dotty.tools.io.AbstractFile
import dotty.tools.io.ClassRepresentation
import dotty.tools.io.FileExtension

case class ClassPathEntries(packages: scala.collection.Seq[PackageEntry], classesAndSources: scala.collection.Seq[ClassRepresentation]) {
  def toTuple: (scala.collection.Seq[PackageEntry], scala.collection.Seq[ClassRepresentation]) = (packages, classesAndSources)
}

object ClassPathEntries {
  val empty = ClassPathEntries(Seq.empty, Seq.empty)
}

case class PackageName(dottedString: String) {
  val dirPathTrailingSlashJar: String = FileUtils.dirPathInJar(dottedString) + "/"

  val dirPathTrailingSlash: String =
    if (java.io.File.separatorChar == '/')
      dirPathTrailingSlashJar
    else
      FileUtils.dirPath(dottedString) + java.io.File.separator

  def isRoot: Boolean = dottedString.isEmpty

  def entryName(entry: String): String = {
    if (isRoot) entry else {
      val builder = new java.lang.StringBuilder(dottedString.length + 1 + entry.length)
      builder.append(dottedString)
      builder.append('.')
      builder.append(entry)
      builder.toString
    }
  }
}

trait PackageEntry {
  def name: String
}

/** A TASTy file or classfile */
sealed trait BinaryFileEntry extends ClassRepresentation {
  def file: AbstractFile
  final def fileName: String = file.name
  final def name: String = FileUtils.stripClassExtension(file.name) // class name
  final def source: Option[AbstractFile] = None
}

object BinaryFileEntry {
  def apply(file: AbstractFile): BinaryFileEntry =
    if file.isTasty then
      if file.resolveSiblingWithExtension(FileExtension.Class) != null then TastyWithClassFileEntry(file)
      else StandaloneTastyFileEntry(file)
    else
      ClassFileEntry(file)
}

/** A classfile or .sig that does not have an associated TASTy file */
private[dotty] final case class ClassFileEntry(file: AbstractFile) extends BinaryFileEntry {
  def binary: Option[AbstractFile] = Some(file)
}

/** A TASTy file that has an associated class file */
private[dotty] final case class TastyWithClassFileEntry(file: AbstractFile) extends BinaryFileEntry {
  def binary: Option[AbstractFile] = Some(file)
}

/** A TASTy file that does not have an associated class file */
private[dotty] final case class StandaloneTastyFileEntry(file: AbstractFile) extends BinaryFileEntry {
  def binary: Option[AbstractFile] = Some(file)
}

private[dotty] final case class SourceFileEntry(file: AbstractFile) extends ClassRepresentation {
  final def fileName: String = file.name
  def name: String = FileUtils.stripSourceExtension(file.name)
  def binary: Option[AbstractFile] = None
  def source: Option[AbstractFile] = Some(file)
}

private[dotty] final case class BinaryAndSourceFilesEntry(binaryEntry: BinaryFileEntry, sourceEntry: SourceFileEntry) extends ClassRepresentation {
  final def fileName: String = binaryEntry.fileName
  def name: String = binaryEntry.name
  def binary: Option[AbstractFile] = binaryEntry.binary
  def source: Option[AbstractFile] = sourceEntry.source
}

private[dotty] case class PackageEntryImpl(name: String) extends PackageEntry

private[dotty] trait NoSourcePaths {
  def asSourcePathString: String = ""
  private[dotty] def sources(inPackage: PackageName): Seq[SourceFileEntry] = Seq.empty
}

private[dotty] trait NoClassPaths {
  def findClassFile(className: String): Option[AbstractFile] = None
  private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry] = Seq.empty
}
