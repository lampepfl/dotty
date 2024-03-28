/** This test checks that provided given instances take precedence over default
  * given arguments, even when there are multiple default arguments.
  *
  * Before the fix for issue #19414, this code would fail with a "No given
  * instance of type BodySerializer[JsObject]".
  *
  * See also:
  *   - tests/neg/19414-desugared.scala
  *   - tests/neg/given-ambiguous-default-1.scala
  *   - tests/neg/given-ambiguous-default-2.scala
  */

trait JsValue
trait JsObject extends JsValue

trait Writer[T]
trait BodySerializer[-B]

class Printer

given Writer[JsValue] = ???
given Writer[JsObject] = ???

given [B: Writer](using printer: Printer = new Printer): BodySerializer[B] = ???

def f: Unit =
  summon[BodySerializer[JsObject]] // error: Ambiguous given instances
