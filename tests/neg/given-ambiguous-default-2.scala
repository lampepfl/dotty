/** This test checks that provided given instances take precedence over default
  * given arguments, even when there are multiple default arguments.
  *
  * Before the fix for issue #19414, this code would compile without errors.
  *
  * See also:
  *   - tests/neg/given-ambiguous-default-1.scala
  *   - tests/neg/19414.scala
  *   - tests/neg/19414-desugared.scala
  */

class A
class B
class C
given a1: A = ???
given a2: A = ???
given (using a: A = A(), b: B = B()): C = ???

def f: Unit = summon[C] // error: Ambiguous given instances
