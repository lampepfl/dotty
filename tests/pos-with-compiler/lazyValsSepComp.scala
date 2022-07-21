package dotty.tools
package io

import java.io.InputStream
import java.util.jar.JarEntry
import dotty.tools.dotc.core.Definitions
import language.postfixOps
import dotty.tools.dotc.core.Contexts.*


/** A test to trigger issue with separate compilation and lazy vals */
object Foo {
  val definitions: Definitions = null.asInstanceOf[Definitions]
  def defn = definitions
  def go = defn.ScalaBoxedClasses
}
