sealed trait Wrap[A <: AnyKind]

trait Foo[T <: AnyKind, X[_ <: T]]

trait Bar[T <: AnyKind, X[_ <: T]] {

  def foo: Foo[
    Wrap[? <: T],
    [x <: Wrap[? <: T]] =>> x match { case Wrap[a] => X[a] }
  ]

}

object Qux extends Bar[Any, [_] =>> Unit] {

  override def foo: Foo[
    Wrap[? <: Any],
    [x <: Wrap[? <: Any]] =>> x match { case Wrap[a] => Unit }
  ] = ???

}
