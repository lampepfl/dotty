//> using options -language:experimental.modularity -source future

class Common:

  trait Ord:
    type Self
    extension (x: Self)
      def compareTo(y: Self): Int
      def < (y: Self): Boolean = compareTo(y) < 0
      def > (y: Self): Boolean = compareTo(y) > 0
      def <= (y: Self): Boolean = compareTo(y) <= 0
      def >= (y: Self): Boolean = compareTo(y) >= 0
      def max(y: Self): Self = if x < y then y else x

  trait Show:
    type Self
    extension (x: Self) def show: String

  trait SemiGroup:
    type Self
    extension (x: Self) def combine(y: Self): Self

  trait Monoid extends SemiGroup:
    def unit: Self

  trait Functor:
    type Self[A]
    extension [A](x: Self[A]) def map[B](f: A => B): Self[B]

  trait Monad extends Functor:
    def pure[A](x: A): Self[A]
    extension [A](x: Self[A])
      def flatMap[B](f: A => Self[B]): Self[B]
      def map[B](f: A => B) = x.flatMap(f `andThen` pure)
end Common

object Instances extends Common:

  given intOrd: Int is Ord with
    extension (x: Int)
      def compareTo(y: Int) =
        if x < y then -1
        else if x > y then +1
        else 0

//  given [T](using tracked val ev: Ord { type Self = T}): Ord { type Self = List[T] } with
  given [T: Ord]: List[T] is Ord with
    extension (xs: List[T]) def compareTo(ys: List[T]): Int = (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)

  given listMonad: List is Monad with
    extension [A](xs: List[A]) def flatMap[B](f: A => List[B]): List[B] =
      xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)

  type Reader[Ctx] = [X] =>> Ctx => X

  given readerMonad[Ctx]: Reader[Ctx] is Monad with
    extension [A](r: Ctx => A) def flatMap[B](f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x

  extension (xs: Seq[String])
    def longestStrings: Seq[String] =
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)

  extension [T](xs: List[T])
    def second = xs.tail.head
    def third = xs.tail.tail.head

  extension [M[_]: Monad, A](xss: M[M[A]])
    def flatten: M[A] =
      xss.flatMap(identity)

  def maximum[T: Ord](xs: List[T]): T =
    xs.reduce(_ `max` _)

  given descending[T: Ord]: T is Ord with
    extension (x: T) def compareTo(y: T) = T.compareTo(y)(x)

  def minimum[T: Ord](xs: List[T]) =
    maximum(xs)(using descending)

  def test(): Unit =
    val xs = List(1, 2, 3)
    println(maximum(xs))
    println(maximum(xs)(using descending))
    println(maximum(xs)(using descending(using intOrd)))
    println(minimum(xs))

// Adapted from the Rust by Example book: https://doc.rust-lang.org/rust-by-example/trait.html
//
//           lines  words  chars
// wc Scala: 28     105     793
// wc Rust : 57     193    1466

trait Animal:
  type Self
  // Associated function signature; `Self` refers to the implementor type.
  def apply(name: String): Self

  // Method signatures; these will return a string.
  extension (self: Self)
    def name: String
    def noise: String
    def talk(): Unit = println(s"$name, $noise")
end Animal

class Sheep(val name: String):
  var isNaked = false
  def shear() =
    if isNaked then
      println(s"$name is already naked...")
    else
      println(s"$name gets a haircut!")
      isNaked = true

given Sheep is Animal with
  def apply(name: String) = Sheep(name)
  extension (self: Self)
    def name: String = self.name
    def noise: String = if self.isNaked then "baaaaah?" else "baaaaah!"
    override def talk(): Unit =
      println(s"$name pauses briefly... $noise")

/*

 - In a type pattern, A <: T, A >: T, A: T, A: _ are all allowed and mean
   T is a fresh type variable (T can start with a capital letter).
 - instance definitions
 - `as m` syntax in context bounds and instance definitions

*/
