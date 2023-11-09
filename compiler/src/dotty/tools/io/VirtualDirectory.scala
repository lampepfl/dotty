/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package dotty.tools.io

import scala.language.unsafeNulls

import scala.collection.mutable
import java.io.{InputStream, OutputStream}
/**
 * An in-memory directory.
 *
 * @author Lex Spoon
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class VirtualDirectory(val name: String, maybeContainer: Option[VirtualDirectory] = None)
extends AbstractFile { outer =>
  def path: String =
    maybeContainer match {
      case None => name
      case Some(parent) => parent.path + '/' + name
    }

  def absolute: AbstractFile = this

  def container: VirtualDirectory = maybeContainer.get
  def isDirectory: Boolean = true
  override def isVirtual: Boolean = true
  val lastModified: Long = System.currentTimeMillis

  override def jpath: JPath = null
  override def input: InputStream = sys.error("directories cannot be read")
  override def output: OutputStream = sys.error("directories cannot be written")

  /** Does this abstract file denote an existing file? */
  def create(): Unit = { unsupported() }

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit = { unsupported() }

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()

  private val files = mutable.Map.empty[String, AbstractFile]

  // the toList is so that the directory may continue to be
  // modified while its elements are iterated
  def iterator(): Iterator[AbstractFile] = files.values.toList.iterator

  override def lookupName(name: String, directory: Boolean): AbstractFile =
    (files get name filter (_.isDirectory == directory)).orNull

  override def fileNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = false)) getOrElse {
      val newFile = new VirtualFile(name, s"$path/$name", Some(this)) {
        override val underlyingSource: Option[AbstractFile] = outer.underlyingSource
      }
      files(name) = newFile
      newFile
    }

  override def subdirectoryNamed(name: String): AbstractFile =
    Option(lookupName(name, directory = true)) getOrElse {
      val dir = new VirtualDirectory(name, Some(this)) {
        override val underlyingSource: Option[AbstractFile] = outer.underlyingSource
      }
      files(name) = dir
      dir
    }

  def clear(): Unit = {
    files.clear()
  }
}
