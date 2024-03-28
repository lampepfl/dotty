/** This test checks that provided given instances take precedence over default
  * given arguments.
  *
  * In the following code, the compiler must report an "Ambiguous implicits"
  * error for the parameter `a`, and must not use its default value.
  *
  * See also:
  *   - tests/neg/19414.scala
  *   - tests/neg/19414-desugared.scala
  *   - tests/neg/given-ambiguous-default-2.scala
  */

class A
class B
given a1: A = ???
given a2: A = ???
given (using a: A = A()): B = ???

def f: Unit = summon[B] // error: Ambiguous given instances
