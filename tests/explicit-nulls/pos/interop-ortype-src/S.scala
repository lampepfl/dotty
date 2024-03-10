// Tests that member finding works on (FlexibleType(T) | S)
class S {
  def foo(a: J | String) = (a match {
    case x: J => J.foo(x: J)
    case y: String => ""
  }).asInstanceOf[J]
}
