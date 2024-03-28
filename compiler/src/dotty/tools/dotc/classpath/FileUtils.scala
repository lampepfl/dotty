/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools
package dotc.classpath

import scala.language.unsafeNulls

import java.io.{File => JFile, FileFilter}
import java.net.URL
import dotty.tools.io.AbstractFile

/**
 * Common methods related to Java files and abstract files used in the context of classpath
 */
object FileUtils {
  extension (file: AbstractFile) {
    def isPackage: Boolean = file.isDirectory && mayBeValidPackage(file.name)

    def isClass: Boolean = !file.isDirectory && hasClassExtension

    def hasClassExtension: Boolean = file.ext.isClass

    def hasTastyExtension: Boolean = file.ext.isTasty

    def isTasty: Boolean = !file.isDirectory && hasTastyExtension

    def isScalaBinary: Boolean = file.isClass || file.isTasty

    def isScalaOrJavaSource: Boolean = !file.isDirectory && file.ext.isScalaOrJava

    // TODO do we need to check also other files using ZipMagicNumber like in scala.tools.nsc.io.Jar.isJarOrZip?
    def isJarOrZip: Boolean = file.ext.isJarOrZip

    /**
     * Safe method returning a sequence containing one URL representing this file, when underlying file exists,
     * and returning given default value in other case
     */
    def toURLs(default: => Seq[URL] = Seq.empty): Seq[URL] = if (file.file == null) default else Seq(file.toURL)

    /**
     * Returns if there is an existing sibling `.tasty` file.
     */
    def hasSiblingTasty: Boolean =
      assert(file.hasClassExtension, s"non-class: $file")
      file.resolveSibling(classNameToTasty(file.name)) != null
  }

  extension (file: JFile) {
    def isPackage: Boolean = file.isDirectory && mayBeValidPackage(file.getName)

    def isClass: Boolean = file.isFile && hasClassExtension

    def hasClassExtension: Boolean = file.getName.endsWith(SUFFIX_CLASS)

    def isTasty: Boolean = file.isFile && file.getName.endsWith(SUFFIX_TASTY)

    /**
     * Returns if there is an existing sibling `.tasty` file.
     */
    def hasSiblingTasty: Boolean =
      assert(file.hasClassExtension, s"non-class: $file")
      val path = file.toPath
      val tastyPath = path.resolveSibling(classNameToTasty(file.getName))
      java.nio.file.Files.exists(tastyPath)

  }

  private val SUFFIX_CLASS = ".class"
  private val SUFFIX_SCALA = ".scala"
  private val SUFFIX_TASTY = ".tasty"
  private val SUFFIX_JAVA = ".java"
  private val SUFFIX_SIG = ".sig"

  def stripSourceExtension(fileName: String): String =
    if (endsScala(fileName)) stripClassExtension(fileName)
    else if (endsJava(fileName)) stripJavaExtension(fileName)
    else throw new FatalError("Unexpected source file ending: " + fileName)

  def dirPath(forPackage: String): String = forPackage.replace('.', JFile.separatorChar)

  def dirPathInJar(forPackage: String): String = forPackage.replace('.', '/')

  inline private def ends (filename:String, suffix:String) = filename.endsWith(suffix) && filename.length > suffix.length

  def endsClass(fileName: String): Boolean =
    ends (fileName, SUFFIX_CLASS) || fileName.endsWith(SUFFIX_SIG)

  def endsScalaOrJava(fileName: String): Boolean =
    endsScala(fileName) || endsJava(fileName)

  def endsJava(fileName: String): Boolean =
    ends (fileName, SUFFIX_JAVA)

  def endsScala(fileName: String): Boolean =
    ends (fileName, SUFFIX_SCALA)

  def stripClassExtension(fileName: String): String =
    fileName.substring(0, fileName.lastIndexOf('.'))

  def stripJavaExtension(fileName: String): String =
    fileName.substring(0, fileName.length - 5) // equivalent of fileName.length - SUFFIX_JAVA.length

  // probably it should match a pattern like [a-z_]{1}[a-z0-9_]* but it cannot be changed
  // because then some tests in partest don't pass
  def mayBeValidPackage(dirName: String): Boolean =
    (dirName != "META-INF") && (dirName != "") && (dirName.charAt(0) != '.')

  def mkFileFilter(f: JFile => Boolean): FileFilter = new FileFilter {
    def accept(pathname: JFile): Boolean = f(pathname)
  }

  /** Transforms a .class file name to a .tasty file name */
  private def classNameToTasty(fileName: String): String =
    val classOrModuleName = fileName.stripSuffix(".class")
    val className =
      if classOrModuleName.endsWith("$")
        && classOrModuleName != "Null$" // scala.runtime.Null$
        && classOrModuleName != "Nothing$" // scala.runtime.Nothing$
        // Special case for `object $` in Amonite.
        // This is an ad-hoc workaround for Amonite `object $`. See issue #19702
        // This definition is not valid Scala.
        && classOrModuleName != "$"
      then classOrModuleName.stripSuffix("$")
      else classOrModuleName
    className + SUFFIX_TASTY
}
