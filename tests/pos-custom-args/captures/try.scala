import annotation.retains
import language.experimental.erasedDefinitions

class CT[E <: Exception]
type CanThrow[E <: Exception] = CT[E] @retains(caps.cap)

infix type throws[R, E <: Exception] = (erased CanThrow[E]) ?-> R

class Fail extends Exception

def raise[E <: Exception](e: E): Nothing throws E = throw e

def foo(x: Boolean): Int throws Fail =
  if x then 1 else raise(Fail())

def handle[E <: Exception, R](op: (erased CanThrow[E]) -> R)(handler: E -> R): R =
  erased val x: CanThrow[E] = ??? : CanThrow[E]
  try op(x)
  catch case ex: E => handler(ex)

val bar = handle { (erased x) =>
    if true then
      raise(new Exception)(using x)
      22
    else
      11
  }