// derived from shapeless 2 tests at https://github.com/milessabin/shapeless/blob/ae9fc166082adb3fa590a604b256c9e87a467c56/core/src/test/scala/shapeless/generic.scala

import shapeless.impl.*

trait Parent {
  case class Nested(i: Int, s: String)

  sealed abstract class Foo extends Product with Serializable

  case object A extends Foo
  case object B extends Foo
  case class C() extends Foo
}

trait Child extends Parent {
  val gen = Generic[Nested]
  val adtGen = Generic[Foo]
}

object O extends Child

def testNestedInherited: Unit = {
  val n0 = O.Nested(23, "foo")
  val repr = O.gen.to(n0)
  typed[Int :: String :: HNil](repr)
  val n1 = O.gen.from(repr)
  typed[O.Nested](n1)
  assertEquals(n0, n1)

  {
    val foo0 = O.B
    val repr = O.adtGen.to(foo0)
    typed[O.A.type :+: O.B.type :+: O.C :+: CNil](repr)
  }

  {
    val foo0 = O.C()
    val repr = O.adtGen.to(foo0)
    typed[O.A.type :+: O.B.type :+: O.C :+: CNil](repr)
  }
}

def testPathViaObject: Unit = {
  sealed trait T
  object T {
    case class C(i: Int) extends T
    case object O extends T
  }

  type Repr = T.C :+: T.O.type :+: CNil
  val gen = Generic[T]
  val c = T.C(42)
  val injC: Repr = Inl(c)
  val injO: Repr = Inr(Inl(T.O))

  assertTypedEquals[Repr](injC, gen.to(c))
  assertTypedEquals[T](c, gen.from(injC))
  assertTypedEquals[Repr](injO, gen.to(T.O))
  assertTypedEquals[T](T.O, gen.from(injO))
}

def testPathViaObjectNoCompanion: Unit = {
  sealed trait T
  object Wrap {
    case class C(i: Int) extends T
    case object O extends T
  }

  type Repr = Wrap.C :+: Wrap.O.type :+: CNil
  val gen = Generic[T]
  val c = Wrap.C(42)
  val injC: Repr = Inl(c)
  val injO: Repr = Inr(Inl(Wrap.O))

  assertTypedEquals[Repr](injC, gen.to(c))
  assertTypedEquals[T](c, gen.from(injC))
  assertTypedEquals[Repr](injO, gen.to(Wrap.O))
  assertTypedEquals[T](Wrap.O, gen.from(injO))
}

def testPathViaSubPrefix: Unit = {
  class Outer {
    class Inner {
      sealed trait T
    }

    val inner = new Inner
    case class C(i: Int) extends inner.T
    case object O extends inner.T

    final lazy val genThis = Generic[inner.T]
  }

  class SubOuter extends Outer {
    lazy val genSubThis = Generic[inner.T]
  }

  val outer = new Outer
  type Repr = outer.C :+: outer.O.type :+: CNil
  val gen = Generic[outer.inner.T]
  val c = outer.C(42)
  val injC: Repr = Inl(c)
  val injO: Repr = Inr(Inl(outer.O))

  assertTypedEquals[Repr](injC, gen.to(c))
  assertTypedEquals[outer.inner.T](c, gen.from(injC))
  assertTypedEquals[Repr](injO, gen.to(outer.O))
  assertTypedEquals[outer.inner.T](outer.O, gen.from(injO))
}

@main def Test =
  testNestedInherited
  testPathViaObject
  testPathViaObjectNoCompanion
  testPathViaSubPrefix

package GenericTestsAux2 {

  class Bar[A]

  object Bar {
    given gen[A](using Generic[A]): Bar[A] = Bar()
  }

  class Outer1 {
    sealed trait Color
    object Inner {
      case object Red extends Color
    }

    implicitly[Bar[Color]]
  }

  object Outer2 {
    // We support this one because the child types are static - so this should have always compiled.
    // if `Outer2` was a trait, this would fail in `whyNotGenericSum`.
    class Wrapper {
      sealed trait Color
    }
    val wrapper = new Wrapper
    import wrapper.Color
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

    implicitly[Bar[Color]]
  }

  object Outer3 {
    class Wrapper {
      sealed trait Color
    }
    val wrapper = new Wrapper
    case object Red extends wrapper.Color
    case object Green extends wrapper.Color
    case object Blue extends wrapper.Color

    implicitly[Bar[wrapper.Color]]
  }

  def outer3Local = {
    class Wrapper {
      object Nested {
        sealed trait Color
      }
    }
    val wrapper = new Wrapper
    import wrapper.Nested.Color
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

    implicitly[Bar[Color]]
  }

  object Outer4 {
    val wrapper = new Wrapper
    case object Red extends wrapper.Color
    case object Green extends wrapper.Color
    case object Blue extends wrapper.Color

    class Wrapper {
      sealed trait Color
      implicitly[Bar[wrapper.Color]]
    }
  }

  object Outer5 {
    trait Command
    object Command {
      sealed trait Execution extends Command
    }
    case class Buzz() extends Command.Execution
    case class Door() extends Command.Execution
    Generic[Command.Execution]
  }
}

object MixedCCNonCCNested {
  // Block local
  {
    object T1{
      sealed abstract class Tree
      final case class Node(left: Tree, right: Tree, v: Int) extends Tree
      case object Leaf extends Tree
    }

    Generic[T1.Tree]
    import T1._
    Generic[Tree]

    sealed trait A
    sealed case class B(i: Int, s: String) extends A
    case object C extends A
    sealed trait D extends A
    final case class E(a: Double, b: Option[Float]) extends D
    case object F extends D
    sealed abstract class Foo extends D
    case object Baz extends Foo
    // final class Bar extends Foo // Mirrors only support case classes
    // final class Baz(val i1: Int, val s1: String) extends Foo  // Mirrors only support case classes

    Generic[A]
    Generic[B]
    Generic[C.type]
    Generic[D]
    Generic[E]
    Generic[F.type]
    Generic[Foo]
    Generic[Baz.type]
    // Generic[Bar] // Mirrors only support case classes
    // Generic[Baz] // Mirrors only support case classes
  }

  def methodLocal: Unit = {
    object T1{
      sealed abstract class Tree
      final case class Node(left: Tree, right: Tree, v: Int) extends Tree
      case object Leaf extends Tree
    }

    Generic[T1.Tree]
    import T1._
    Generic[Tree]

    sealed trait A
    sealed case class B(i: Int, s: String) extends A
    case object C extends A
    sealed trait D extends A
    final case class E(a: Double, b: Option[Float]) extends D
    case object F extends D
    sealed abstract class Foo extends D
    case object Baz extends Foo
    // final class Bar extends Foo // Mirrors only support case classes
    // final class Baz(val i1: Int, val s1: String) extends Foo // Mirrors only support case classes

    Generic[A]
    Generic[B]
    Generic[C.type]
    Generic[D]
    Generic[E]
    Generic[F.type]
    Generic[Foo]
    Generic[Baz.type]
    // Generic[Bar] // Mirrors only support case classes
    // Generic[Baz] // Mirrors only support case classes
  }
}

package TestPrefixes1 {
  trait Defs {
    case class CC(i: Int, s: String)

    sealed trait Sum
    case class SumI(i: Int) extends Sum
    case class SumS(s: String) extends Sum
  }

  object Defs extends Defs

  object Derivations {
    import shapeless.impl._

    Generic[Defs.CC]
    Generic[Defs.SumI]
    Generic[Defs.SumS]

    Generic[Defs.Sum]
    Generic.materialize[Defs.Sum, Defs.SumI :+: Defs.SumS :+: CNil]
  }
}

object PrivateCtorDefns {
  sealed trait PublicFamily
  case class PublicChild() extends PublicFamily
  private case class PrivateChild() extends PublicFamily

  case class WrongApplySignature private(value: String)
  object WrongApplySignature {
    def apply(v: String): Either[String, WrongApplySignature] = Left("No ways")
  }
}

object PrivateCtor {
  import PrivateCtorDefns._

  Generic[PublicFamily] // in shapeless this was commented out, but it works here
  Generic[WrongApplySignature] // in shapeless this was commented out, but it works here
}

////// ADAPTER CODE ////////

package shapeless {
  object impl {
    import scala.deriving.Mirror

    def assertEquals(a: Any, b: Any): Unit =
      assert(a == b, s"$a != $b")

    def assertTypedEquals[T](a: T, b: T): Unit =
      assertEquals(a, b)

    def typed[T](expr: T): Unit = ()

    type HList = Tuple
    type HNil = EmptyTuple
    type ::[+H, +T <: HList] = *:[H, T]

    case class CList(e: Any)
    class CNil private () extends CList(EmptyTuple)
    class :+:[+H, +T <: CList](elem: Any) extends CList(elem)

    def Inl[E](elem: E): E :+: Nothing = :+:(elem)
    def Inr[Elems <: CList](elems: Elems): Nothing :+: Elems = :+:(elems.e)

    type CListRefl[T <: Tuple] <: CList = T match {
      case EmptyTuple => CNil
      case h *: tl => h :+: CListRefl[tl]
    }

    extension [T](gen: Generic[T])(using m: Mirror.Of[T])
      transparent inline def to(t: T) =
        inline compiletime.erasedValue[m.type] match
          case _: Mirror.ProductOf[T] =>
            Tuple.fromProduct(t.asInstanceOf[Product]).asInstanceOf[m.MirroredElemTypes]
          case _: Mirror.SumOf[T] =>
            Inl(t).asInstanceOf[CListRefl[m.MirroredElemTypes]]

      transparent inline def from[R](r: R) =
        inline compiletime.erasedValue[m.type] match
          case _: Mirror.ProductOf[T] =>
            m.asInstanceOf[Mirror.ProductOf[T]].fromProduct(r.asInstanceOf[Product]).asInstanceOf[T]
          case _: Mirror.SumOf[T] =>
            r.asInstanceOf[CList].e.asInstanceOf[T]

    trait Generic[T]:
      type Repr

    object Generic:

      type Repr[T, M, Elems <: Tuple] = M match
        case Mirror.Sum { type MirroredType = T } => CListRefl[Elems]
        case Mirror.Product { type MirroredType = T } => Elems

      transparent inline given [T](using m: Mirror.Of[T]): Generic[T] = apply[T]

      transparent inline def apply[T](using m: Mirror.Of[T]) = new Generic[T] {
        type Repr = Generic.Repr[T, m.type, m.MirroredElemTypes]
      }

      transparent inline def materialize[T, R](using m: Mirror.Of[T])(using
        R <:< Repr[T, m.type, m.MirroredElemTypes]): Unit = ()
  }
}
