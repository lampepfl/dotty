/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools
package dotc.classpath

import scala.language.unsafeNulls

import java.net.URL
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ArraySeq
import dotc.util

import dotty.tools.io.{ AbstractFile, ClassPath, ClassRepresentation, EfficientClassPath }

/**
 * A classpath unifying multiple class- and sourcepath entries.
 * The Classpath can obtain entries for classes and sources independently
 * so it tries to do operations quite optimally - iterating only these collections
 * which are needed in the given moment and only as far as it's necessary.
 *
 * @param aggregates classpath instances containing entries which this class processes
 */
case class AggregateClassPath(aggregates: Seq[ClassPath]) extends ClassPath {
  override def findClassFile(className: String): Option[AbstractFile] = {
    val (pkg, _) = PackageNameUtils.separatePkgAndClassNames(className)
    aggregatesForPackage(PackageName(pkg)).iterator.map(_.findClassFile(className)).collectFirst {
      case Some(x) => x
    }
  }
  private val packageIndex: collection.mutable.Map[String, Seq[ClassPath]] = collection.mutable.Map()
  private def aggregatesForPackage(pkg: PackageName): Seq[ClassPath] = packageIndex.synchronized {
    packageIndex.getOrElseUpdate(pkg.dottedString, aggregates.filter(_.hasPackage(pkg)))
  }

  override def asURLs: Seq[URL] = aggregates.flatMap(_.asURLs)

  override def asClassPathStrings: Seq[String] = aggregates.map(_.asClassPathString).distinct

  override def asSourcePathString: String = ClassPath.join(aggregates map (_.asSourcePathString)*)

  override private[dotty] def packages(inPackage: PackageName): Seq[PackageEntry] = {
    val aggregatedPackages = aggregates.flatMap(_.packages(inPackage)).distinct
    aggregatedPackages
  }

  override private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry] =
    getDistinctEntries(_.classes(inPackage))

  override private[dotty] def sources(inPackage: PackageName): Seq[SourceFileEntry] =
    getDistinctEntries(_.sources(inPackage))

  override private[dotty] def hasPackage(pkg: PackageName): Boolean = aggregates.exists(_.hasPackage(pkg))
  override private[dotty] def list(inPackage: PackageName): ClassPathEntries = {
    val packages: java.util.HashSet[PackageEntry] = new java.util.HashSet[PackageEntry]()
    val classesAndSourcesBuffer = collection.mutable.ArrayBuffer[ClassRepresentation]()
    val onPackage: PackageEntry => Unit = packages.add(_)
    val onClassesAndSources: ClassRepresentation => Unit = classesAndSourcesBuffer += _

    aggregates.foreach { cp =>
      try {
        cp match {
          case ecp: EfficientClassPath =>
            ecp.list(inPackage, onPackage, onClassesAndSources)
          case _ =>
            val entries = cp.list(inPackage)
            entries._1.foreach(entry => packages.add(entry))
            classesAndSourcesBuffer ++= entries._2
        }
      } catch {
        case ex: java.io.IOException =>
          val e = FatalError(ex.getMessage)
          e.initCause(ex)
          throw e
      }
    }

    val distinctPackages: Seq[PackageEntry] = {
      val arr = packages.toArray(new Array[PackageEntry](packages.size()))
      ArraySeq.unsafeWrapArray(arr)
    }
    val distinctClassesAndSources = mergeClassesAndSources(classesAndSourcesBuffer)
    ClassPathEntries(distinctPackages, distinctClassesAndSources)
  }

  /** Returns only one entry for each name.
   *
   *  If there's both a source and a class entry, it
   *  creates an entry containing both of them. If there would be more than one class or source
   *  entries for the same class it always would use the first entry of each type found on a classpath.
   *
   *  A TASTy file with no class file entry will be chosen over a class file entry. This can happen if we load
   *  the Scala 2 library as it has one JAR containing the class files and one JAR containing the TASTy files.
   *  As classpath orders are not guaranteed to be deterministic we might end up having the TASTy in a later classpath entry.
   */
  private def mergeClassesAndSources(entries: scala.collection.Seq[ClassRepresentation]): Seq[ClassRepresentation] = {
    // based on the implementation from MergedClassPath
    var count = 0
    val indices = util.HashMap[String, Int]()
    val mergedEntries = new ArrayBuffer[ClassRepresentation](entries.size)
    for {
      entry <- entries
    } {
      val name = entry.name
      if (indices.contains(name)) {
        val index = indices(name)
        val existing = mergedEntries(index)
        (entry, existing) match
          case (entry: SourceFileEntry, existing: BinaryFileEntry) =>
            mergedEntries(index) = BinaryAndSourceFilesEntry(existing, entry)
          case (entry: BinaryFileEntry, existing: SourceFileEntry) =>
            mergedEntries(index) = BinaryAndSourceFilesEntry(entry, existing)
          case (entry: StandaloneTastyFileEntry, _: ClassFileEntry) =>
            // Here we do not create a TastyWithClassFileEntry because the TASTy and the classfile
            // come from different classpaths. These may not have the same TASTy UUID.
            mergedEntries(index) = entry
          case (entry: StandaloneTastyFileEntry, BinaryAndSourceFilesEntry(_: ClassFileEntry, sourceEntry)) =>
            mergedEntries(index) = BinaryAndSourceFilesEntry(entry, sourceEntry)
          case _ =>
      }
      else {
        indices(name) = count
        mergedEntries += entry
        count += 1
      }
    }
    if (mergedEntries.isEmpty) Nil else mergedEntries.toIndexedSeq
  }

  private def getDistinctEntries[EntryType <: ClassRepresentation](getEntries: ClassPath => Seq[EntryType]): Seq[EntryType] = {
    val seenNames = util.HashSet[String]()
    val entriesBuffer = new ArrayBuffer[EntryType](1024)
    for {
      cp <- aggregates
      entry <- getEntries(cp) if !seenNames.contains(entry.name)
    }
    {
      entriesBuffer += entry
      seenNames += entry.name
    }
    entriesBuffer.toIndexedSeq
  }
}

object AggregateClassPath {
  def createAggregate(parts: ClassPath*): ClassPath = {
    val elems = new ArrayBuffer[ClassPath]()
    parts foreach {
      case AggregateClassPath(ps) => elems ++= ps
      case p => elems += p
    }
    if (elems.size == 1) elems.head
    else AggregateClassPath(elems.toIndexedSeq)
  }
}
