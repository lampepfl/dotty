package dotty.tools
package dotc
package config

import io.{ClassPath, AbstractFile}
import core.Contexts._, core.Symbols._
import core.SymbolLoader
import core.StdNames.nme
import core.Flags.Module

/** The platform dependent pieces of Global.
 */
abstract class Platform {

  /** The root symbol loader. */
  def rootLoader(root: TermSymbol)(using Context): SymbolLoader

  /** The compiler classpath. */
  def classPath(using Context): ClassPath

  /** Update classpath with a substitution that maps entries to entries */
  def updateClassPath(subst: Map[ClassPath, ClassPath]): Unit

  /** Any platform-specific phases. */
  //def platformPhases: List[SubComponent]

  /** Is the SAMType `cls` also a SAM under the rules of the platform? */
  def isSam(cls: ClassSymbol)(using Context): Boolean

  /** The various ways a boxed primitive might materialize at runtime. */
  def isMaybeBoxed(sym: ClassSymbol)(using Context): Boolean

  /** Is the given class symbol eligible for Java serialization-specific methods? */
  def shouldReceiveJavaSerializationMethods(sym: ClassSymbol)(using Context): Boolean

  /** Create a new class loader to load class file `bin` */
  def newClassLoader(bin: AbstractFile)(using Context): SymbolLoader

  /** Create a new TASTy loader to load class file `bin` */
  def newTastyLoader(bin: AbstractFile)(using Context): SymbolLoader

  /** The given symbol is a method with the right name and signature to be a runnable program. */
  def isMainMethod(sym: Symbol)(using Context): Boolean

  /** The given class has a main method. */
  final def hasMainMethod(sym: Symbol)(using Context): Boolean =
    sym.info.member(nme.main).hasAltWith(d =>
      isMainMethod(d.symbol) && (sym.is(Module) || d.symbol.isStatic))
}
