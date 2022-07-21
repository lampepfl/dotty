// Inspired by the original Fileish,
// testing combinations of lazy and non-lazy vals for their treatment in constructors
package dotty.tools
package io

import java.io.InputStream
import java.util.jar.JarEntry
import language.postfixOps

/** A common interface for File-based things and Stream-based things.
 *  (In particular, io.File and JarEntry.)
 */
class Fileish(val path: Path, val input: () => InputStream) extends Streamable.Chars {
  def inputStream() = input()

  def parent       = path.parent
  def name         = path.name
  def isSourceFile = path.hasExtension("java", "scala")

  private lazy val pkgLines = lines() collect { case x if x startsWith "package " => x stripPrefix "package" trim }
  lazy val pkgFromPath      = parent.path.replaceAll("""[/\\]""", ".")
  lazy val pkgFromSource    = pkgLines map (_.nn.stripSuffix(";")) mkString "."

  override def toString = path.path
}
class Fileish2(val path: Path, val input: () => InputStream) extends Streamable.Chars {
  def inputStream() = input()

  def parent       = path.parent
  def name         = path.name
  def isSourceFile = path.hasExtension("java", "scala")

  private val pkgLines = lines() collect { case x if x startsWith "package " => x stripPrefix "package" trim }
  lazy val pkgFromPath      = parent.path.replaceAll("""[/\\]""", ".")
  lazy val pkgFromSource    = pkgLines map (_.nn.stripSuffix(";")) mkString "."

  override def toString = path.path
}

class Fileish3(val path: Path, val input: () => InputStream) extends Streamable.Chars {
  def inputStream() = input()

  def parent       = path.parent
  def name         = path.name
  def isSourceFile = path.hasExtension("java", "scala")

  private val pkgLines = lines() collect { case x if x startsWith "package " => x stripPrefix "package" trim }
  private val pkgFromPath      = parent.path.replaceAll("""[/\\]""", ".")
  private val pkgFromSource    = pkgLines map (_.nn.stripSuffix(";")) mkString "."

  override def toString = path.path
}

