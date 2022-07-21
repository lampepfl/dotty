class B:
  def g: String | Null = ???

  def f =
    import scala.language.unsafeNulls
    if ??? then "" else g

import scala.language.unsafeNulls
class C {
  def g: String | Null = ???

  def f =
    if ??? then "" else g
}
