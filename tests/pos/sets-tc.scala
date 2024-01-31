import language.experimental.modularity

// First version: higher-kinded self type
object v1:
  trait Sets:
    type Self[A]
    def empty[A]: Self[A]
    def union[A](self: Self[A], other: Self[A]): Self[A]

  case class ListSet[A](elems: List[A])

  given ListSet forms Sets:
    def empty[A]: ListSet[A] = ListSet(Nil)

    def union[A](self: ListSet[A], other: ListSet[A]): ListSet[A] =
      ListSet(self.elems ++ other.elems)

  def listUnion[A, S[_]: Sets](xs: List[S[A]]): S[A] =
    xs.foldLeft(S.empty)(S.union)

  val xs = ListSet(List(1, 2, 3))
  val ys = ListSet(List(4, 5))
  val zs = listUnion(List(xs, ys))

  // Second version: parameterized type class
object v2:
  trait Sets[A]:
    type Self
    def empty: Self
    extension (s: Self) def union (other: Self): Self

  case class ListSet[A](elems: List[A])

  given [A] => ListSet[A] forms Sets[A]:
    def empty: ListSet[A] = ListSet(Nil)

    extension (self: ListSet[A]) def union(other: ListSet[A]): ListSet[A] =
      ListSet(self.elems ++ other.elems)

  def listUnion[A, S: Sets[A]](xs: List[S]): S =
    xs.foldLeft(S.empty)(_ `union` _)

  val xs = ListSet(List(1, 2, 3))
  val ys = ListSet(List(4, 5))
  val zs = listUnion(List(xs, ys))

