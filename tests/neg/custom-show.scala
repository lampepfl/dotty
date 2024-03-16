import scala.compiletime.{CustomShow, hasCustomShow}
import language.experimental

object Regular:
  trait Foo[T]
  type Baz = Foo["To Infinity And Beyond!"]
  val x: Foo["To Infinity And Beyond!"] = 1 // error

object Custom1:
  @hasCustomShow
  trait Foo[T]
  type Baz = Foo["To Infinity And Beyond!"]
  object Foo:
    given CustomShow[Baz] with {
      type Out = "Baz"
    }
  val x: Foo["To Infinity And Beyond!"] = 1 // error

object Custom2:
  @hasCustomShow
  opaque type Foo[T] = 0
  type Baz = Foo["To Infinity And Beyond!"]
  given CustomShow[Baz] with {
    type Out = "Baz"
  }
  val x: Foo["To Infinity And Beyond!"] = 1 // error

object CustomMissingImplicit:
  @hasCustomShow
  trait Foo[T]
  type Baz = Foo["To Infinity And Beyond!"]
  val x: Foo["To Infinity And Beyond!"] = 1 // error