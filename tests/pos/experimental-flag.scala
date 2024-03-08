//> using options -experimental -Yno-experimental

import scala.annotation.experimental

class Foo:
  def foo: Int = experimentalDef

class Bar:
  def bar: Int = experimentalDef
object Bar:
  def bar: Int = experimentalDef

object Baz:
  def bar: Int = experimentalDef

def toplevelMethod: Int = experimentalDef

@experimental def experimentalDef: Int = 1
