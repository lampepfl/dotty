def f1(s: Int): String = "a"
def f1: Int => String = _ => "b"

def f2(s: Int): String = "a"
val f2: Int => String = _ => "b"

def f3(s: Int): String = "a"
def f3(): Int => String = _ => "b"

object DefDef:
  def f4(s: Int): String = "a"
  def f4: Int => String = _ => "b"
object DefVal:
  def f5(s: Int): String = "a"
  val f5: Int => String = _ => "b"

@main def Test =
  val test01: Int => String = f1 // error
  val test02: Int => String = f2 // error
  val test03: Int => String = f3

  val test1: Int => String = DefDef.f4 // error
  def dtest1: Int => String = DefDef.f4 // error

  val test2: Int => String = DefVal.f5 // error
  def dtest2: Int => String = DefVal.f5 // error
