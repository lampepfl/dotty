package dotty.tools
package dotc
package core
package pickling

import core.Names.TermName
import collection.mutable

abstract class TastyName

object TastyName {
  
  case class NameRef(val index: Int) extends AnyVal
  
  case class Simple(name: TermName) extends TastyName
  case class Qualified(qualified: NameRef, selector: NameRef) extends TastyName
  case class Signed(original: NameRef, params: List[NameRef], result: NameRef) extends TastyName 
  case class Expanded(original: NameRef) extends TastyName
  case class ModuleClass(module: NameRef) extends TastyName
  case class SuperAccessor(accessed: NameRef) extends TastyName
  case class DefaultGetter(method: NameRef, num: Int) extends TastyName
  
  class Table extends (NameRef => TastyName) {
    private val names = new mutable.ArrayBuffer[TastyName]
    def add(name: TastyName) = names += name
    def apply(ref: NameRef) = names(ref.index)
    def contents: Iterable[TastyName] = names
  }
}  
