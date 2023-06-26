inline trait A:
  given List[String] = "AAA" :: Nil
  def foo[T: List](x: T): T = summon[List[T]].headOption.getOrElse(x)

class B extends A:
  def f = foo("BBB")