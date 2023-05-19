

class Foo {
  def unary_~() : Foo = this // warn
  def unary_-(using Int)(): Foo = this // warn
  def unary_+()(implicit i: Int): Foo = this // warn
  def unary_![T](): Foo = this // warn
}

class Bar {
  def unary_~ : Bar = this
  def unary_-(using Int): Bar = this
  def unary_+(implicit i: Int): Bar = this
  def unary_![T]: Bar = this
}

final class Baz private (val x: Int) extends AnyVal {
  def unary_- : Baz = ???
  def unary_+[T] : Baz = ???
  def unary_!() : Baz = ??? // warn
  def unary_~(using Int) : Baz = ???
}

extension (x: Int)
  @annotation.nowarn
  def unary_- : Int = ???
  @annotation.nowarn
  def unary_+[T] : Int = ???
  def unary_!() : Int = ??? // warn
  @annotation.nowarn
  def unary_~(using Int) : Int = ???
end extension

extension [T](x: Short)
  @annotation.nowarn
  def unary_- : Int = ???
  @annotation.nowarn
  def unary_+[U] : Int = ???
  def unary_!() : Int = ??? // warn
  @annotation.nowarn
  def unary_~(using Int) : Int = ???
end extension

extension (using Int)(x: Byte)
  @annotation.nowarn
  def unary_- : Int = ???
  @annotation.nowarn
  def unary_+[U] : Int = ???
  def unary_!() : Int = ??? // warn
  @annotation.nowarn
  def unary_~(using Int) : Int = ???
end extension
