//> using options -source future -language:experimental.modularity
trait Ord:
  type Self
  extension (x: Self)
    def compareTo(y: Self): Int
    def < (y: Self): Boolean = compareTo(y) < 0
    def > (y: Self): Boolean = compareTo(y) > 0

  trait OrdProxy extends Ord:
    export Ord.this.*

trait SemiGroup:
  type Self
  extension (x: Self) def combine(y: Self): Self

  trait SemiGroupProxy extends SemiGroup:
    export SemiGroup.this.*

trait Monoid extends SemiGroup:
  def unit: Self

  trait MonoidProxy extends Monoid:
    export Monoid.this.*

def ordWithMonoid(ord: Ord, monoid: Monoid{ type Self = ord.Self }): Ord & Monoid =
  new ord.OrdProxy with monoid.MonoidProxy {}

trait OrdWithMonoid extends Ord, Monoid

def ordWithMonoid2(ord: Ord, monoid: Monoid{ type Self = ord.Self }) = //: OrdWithMonoid { type Self = ord.Self} =
  new OrdWithMonoid with ord.OrdProxy with monoid.MonoidProxy {}

given intOrd: (Ord { type Self = Int }) = ???
given intMonoid: (Monoid { type Self = Int }) = ???

//given (using ord: Ord, monoid: Monoid{ type Self = ord.Self }): (Ord & Monoid { type Self = ord.Self}) =
//  ordWithMonoid2(ord, monoid)

val x = summon[Ord & Monoid { type Self = Int}]
val y: Int = ??? : x.Self

// given [A, B](using ord: A is Ord, monoid: A is Monoid) => A is Ord & Monoid =
//   new ord.OrdProxy with monoid.MonoidProxy {}

given [A](using ord: Ord { type Self = A }, monoid: Monoid { type Self = A}): ((Ord & Monoid) { type Self = A}) =
  new ord.OrdProxy with monoid.MonoidProxy {}

