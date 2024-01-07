//> using options -source future -language:experimental.modularity
trait Ord:
  type This
  extension (x: This)
    def compareTo(y: This): Int
    def < (y: This): Boolean = compareTo(y) < 0
    def > (y: This): Boolean = compareTo(y) > 0

  trait OrdProxy extends Ord:
    export Ord.this.*

trait SemiGroup:
  type This
  extension (x: This) def combine(y: This): This

  trait SemiGroupProxy extends SemiGroup:
    export SemiGroup.this.*

trait Monoid extends SemiGroup:
  def unit: This

  trait MonoidProxy extends Monoid:
    export Monoid.this.*

def ordWithMonoid(ord: Ord, monoid: Monoid{ type This = ord.This }): Ord & Monoid =
  new ord.OrdProxy with monoid.MonoidProxy {}

trait OrdWithMonoid extends Ord, Monoid

def ordWithMonoid2(ord: Ord, monoid: Monoid{ type This = ord.This }) = //: OrdWithMonoid { type This = ord.This} =
  new OrdWithMonoid with ord.OrdProxy with monoid.MonoidProxy {}

given intOrd: (Ord { type This = Int }) = ???
given intMonoid: (Monoid { type This = Int }) = ???

//given (using ord: Ord, monoid: Monoid{ type This = ord.This }): (Ord & Monoid { type This = ord.This}) =
//  ordWithMonoid2(ord, monoid)

val x = summon[Ord & Monoid { type This = Int}]
val y: Int = ??? : x.This

// given [A, B](using ord: A is Ord, monoid: A is Monoid) => A is Ord & Monoid =
//   new ord.OrdProxy with monoid.MonoidProxy {}

given [A](using ord: Ord { type This = A }, monoid: Monoid { type This = A}): ((Ord & Monoid) { type This = A}) =
  new ord.OrdProxy with monoid.MonoidProxy {}

