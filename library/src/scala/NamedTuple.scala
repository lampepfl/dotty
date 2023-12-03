package scala
import annotation.experimental
import compiletime.ops.boolean.*

@experimental
object NamedTuple:

  opaque type AnyNamedTuple = Any
  opaque type NamedTuple[N <: Tuple, +V <: Tuple] >: V <: AnyNamedTuple = V

  def apply[N <: Tuple, V <: Tuple](x: V) = x

  def unapply[N <: Tuple, V <: Tuple](x: NamedTuple[N, V]): Some[V] = Some(x)

  extension [V <: Tuple](x: V)
    inline def withNames[N <: Tuple]: NamedTuple[N, V] = x

  extension [N <: Tuple, V <: Tuple](x: NamedTuple[N, V])

    /** The underlying tuple without the names */
    inline def toTuple: V = x

    /** The number of elements in this tuple */
    inline def size: Tuple.Size[V] = toTuple.size

    // This intentionally works for empty named tuples as well. I think NnEmptyTuple is a dead end
    // and should be reverted, justy like NonEmptyList is also appealing at first, but a bad idea
    // in the end.

    /** The value (without the name) at index `n` of this tuple */
    inline def apply(n: Int): Tuple.Elem[V, n.type] =
      inline toTuple match
        case tup: NonEmptyTuple => tup(n).asInstanceOf[Tuple.Elem[V, n.type]]
        case tup => tup.productElement(n).asInstanceOf[Tuple.Elem[V, n.type]]

    /** The first element value of this tuple */
    inline def head: Tuple.Elem[V, 0] = apply(0)

    /** The tuple consisting of all elements of this tuple except the first one */
    inline def tail: Tuple.Drop[V, 1] = toTuple.drop(1)

    /** The last element value of this tuple */
    inline def last: Tuple.Last[V] = apply(size - 1).asInstanceOf[Tuple.Last[V]]

    /** The tuple consisting of all elements of this tuple except the last one */
    inline def init: Tuple.Init[V] = toTuple.take(size - 1).asInstanceOf[Tuple.Init[V]]

    /** The tuple consisting of the first `n` elements of this tuple, or all
     *  elements if `n` exceeds `size`.
     */
    inline def take(n: Int): NamedTuple[Tuple.Take[N, n.type], Tuple.Take[V, n.type]] =
      toTuple.take(n)

    /** The tuple consisting of all elements of this tuple except the first `n` ones,
     *  or no elements if `n` exceeds `size`.
     */
    inline def drop(n: Int): NamedTuple[Tuple.Drop[N, n.type], Tuple.Drop[V, n.type]] =
      toTuple.drop(n)

    /** The tuple `(x.take(n), x.drop(n))` */
    inline def splitAt(n: Int): NamedTuple[Tuple.Split[N, n.type], Tuple.Split[V, n.type]] =
      toTuple.splitAt(n)

    /** The tuple consisting of all elements of this tuple followed by all elements
     *  of tuple `that`. The names of the two tuples must be disjoint.
     */
    inline def ++ [N2 <: Tuple, V2 <: Tuple](that: NamedTuple[N2, V2])(using Tuple.Disjoint[N, N2] =:= true)
      : NamedTuple[Tuple.Concat[N, N2], Tuple.Concat[V, V2]]
      = toTuple ++ that.toTuple

    // inline def :* [L] (x: L): NamedTuple[Append[N, ???], Append[V, L] = ???
    // inline def *: [H] (x: H): NamedTuple[??? *: N], H *: V] = ???

    /** The named tuple consisting of all element values of this tuple mapped by
     *  the polymorphic mapping function `f`. The names of elements are preserved.
     *  If `x = (n1 = v1, ..., ni = vi)` then `x.map(f) = `(n1 = f(v1), ..., ni = f(vi))`.
     */
    inline def map[F[_]](f: [t] => t => F[t]): NamedTuple[N, Tuple.Map[V, F]] =
      toTuple.map(f).asInstanceOf[NamedTuple[N, Tuple.Map[V, F]]]

    /** The named tuple consisting of all elements of this tuple in reverse */
    inline def reverse: NamedTuple[Tuple.Reverse[N], Tuple.Reverse[V]] =
      toTuple.reverse

    /** The named tuple consisting of all elements values of this tuple zipped
     *  with corresponding element values in named tuple `that`.
     *  If the two tuples have different sizes,
     *  the extra elements of the larger tuple will be disregarded.
     *  The names of `x` and `that` at the same index must be the same.
     *  The result tuple keeps the same names as the operand tuples.
     */
    inline def zip[V2 <: Tuple](that: NamedTuple[N, V2]): NamedTuple[N, Tuple.Zip[V, V2]] =
      toTuple.zip(that.toTuple)

    /** A list consisting of all element values */
    inline def toList: List[Tuple.Union[V]] = toTuple.toList.asInstanceOf[List[Tuple.Union[V]]]

    /** An array consisting of all element values */
    inline def toArray: Array[Object] = toTuple.toArray

    /** An immutable array consisting of all element values */
    inline def toIArray: IArray[Object] = toTuple.toIArray

  end extension

  /** The names of a named tuple, represented as a tuple of literal string values. */
  type Names[X <: AnyNamedTuple] <: Tuple = X match
    case NamedTuple[n, _] => n

  /** The value types of a named tuple represented as a regular tuple. */
  type DropNames[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[_, x] => x

  /** The size of a named tuple, represented as a literal constant subtype of Int */
  type Size[X <: AnyNamedTuple] = Tuple.Size[DropNames[X]]

  /** The type of the element value at position N in the named tuple X */
  type Elem[X <: AnyNamedTuple, N <: Int] = Tuple.Elem[DropNames[X], N]

  /** The type of the first element value of a named tuple */
  type Head[X <: AnyNamedTuple] = Elem[X, 0]

  /** The type of the last element value of a named tuple */
  type Last[X <: AnyNamedTuple] = Tuple.Last[DropNames[X]]

  /** The type of a named tuple consisting of all elements of named tuple X except the first one */
  type Tail[X <: AnyNamedTuple] = Drop[X, 1]

  /** The type of the initial part of a named tuple without its last element */
  type Init[X <: AnyNamedTuple] =
    NamedTuple[Tuple.Init[Names[X]], Tuple.Init[DropNames[X]]]

  /** The type of the named tuple consisting of the first `N` elements of `X`,
   *  or all elements if `N` exceeds `Size[X]`.
   */
  type Take[X <: AnyNamedTuple, N <: Int] =
    NamedTuple[Tuple.Take[Names[X], N], Tuple.Take[DropNames[X], N]]

  /** The type of the named tuple consisting of all elements of `X` except the first `N` ones,
   *  or no elements if `N` exceeds `Size[X]`.
   */
  type Drop[X <: AnyNamedTuple, N <: Int] =
    NamedTuple[Tuple.Drop[Names[X], N], Tuple.Drop[DropNames[X], N]]

  /** The pair type `(Take(X, N), Drop[X, N]). */
  type Split[X <: AnyNamedTuple, N <: Int] = (Take[X, N], Drop[X, N])

  /** Type of the concatenation of two tuples `X` and `Y` */
  type Concat[X <: AnyNamedTuple, Y <: AnyNamedTuple] =
    NamedTuple[Tuple.Concat[Names[X], Names[Y]], Tuple.Concat[DropNames[X], DropNames[Y]]]

  /** The type of the named tuple `X` mapped with the type-level function `F`.
   *  If `X = (n1 : T1, ..., ni : Ti)` then `Map[X, F] = `(n1 : F[T1], ..., ni : F[Ti])`.
   */
  type Map[X <: AnyNamedTuple, F[_ <: Tuple.Union[DropNames[X]]]] =
    NamedTuple[Names[X], Tuple.Map[DropNames[X], F]]

  /** A named tuple with the elements of tuple `X` in reversed order */
  type Reverse[X <: AnyNamedTuple] =
    NamedTuple[Tuple.Reverse[Names[X]], Tuple.Reverse[DropNames[X]]]

  /** The type of the named tuple consisting of all element values of
   *  named tuple `X` zipped with corresponding element values of
   *  named tuple `Y`. If the two tuples have different sizes,
   *  the extra elements of the larger tuple will be disregarded.
   *  The names of `X` and `Y` at the same index must be the same.
   *  The result tuple keeps the same names as the operand tuples.
   *  For example, if
   *  ```
   *     X = (n1 : S1, ..., ni : Si)
   *     Y = (n1 : T1, ..., nj : Tj)  where j >= i
   *  ```
   *  then
   *  ```
   *     Zip[X, Y] = (n1 : (S1, T1), ..., ni: (Si, Ti))
   *  ```
   *  @syntax markdown
   */
  type Zip[X <: AnyNamedTuple, Y <: AnyNamedTuple] =
    Tuple.Conforms[Names[X], Names[Y]] match
      case true =>
        NamedTuple[Names[X], Tuple.Zip[DropNames[X], DropNames[Y]]]

end NamedTuple
