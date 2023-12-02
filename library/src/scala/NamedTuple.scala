package scala
import annotation.experimental
import compiletime.ops.boolean.*

@experimental
object NamedTuple:

  opaque type AnyNamedTuple = Any
  opaque type NamedTuple[N <: Tuple, V <: Tuple] >: V <: AnyNamedTuple = V

  def apply[N <: Tuple, V <: Tuple](x: V) = x

  def unapply[N <: Tuple, V <: Tuple](x: NamedTuple[N, V]): Some[V] = Some(x)

  extension [V <: Tuple](x: V)
    inline def withNames[N <: Tuple]: NamedTuple[N, V] = x

  extension [N <: Tuple, V <: Tuple](x: NamedTuple[N, V])

    inline def values: V = x

    inline def size: Tuple.Size[V] = values.size

    // This intentionally works for empty named tuples as well. I think NnEmptyTuple is a dead end
    // and should be reverted, justy like NonEmptyList is also appealing at first, but a bad idea
    // in the end.
    inline def apply(n: Int): Tuple.Elem[V, n.type] =
      inline values match
        case tup: NonEmptyTuple => tup(n).asInstanceOf[Tuple.Elem[V, n.type]]
        case tup => tup.productElement(n).asInstanceOf[Tuple.Elem[V, n.type]]

    inline def head: Tuple.Elem[V, 0] = apply(0)
    inline def tail: Tuple.Drop[V, 1] = values.drop(1)

    inline def last: Tuple.Last[V] = apply(size - 1).asInstanceOf[Tuple.Last[V]]
    inline def init: Tuple.Init[V] = values.take(size - 1).asInstanceOf[Tuple.Init[V]]

    inline def take(n: Int): NamedTuple[Tuple.Take[N, n.type], Tuple.Take[V, n.type]] =
      values.take(n)

    inline def drop(n: Int): NamedTuple[Tuple.Drop[N, n.type], Tuple.Drop[V, n.type]] =
      values.drop(n)

    inline def splitAt(n: Int): NamedTuple[Tuple.Split[N, n.type], Tuple.Split[V, n.type]] =
      values.splitAt(n)

    inline def ++ [N2 <: Tuple, V2 <: Tuple](that: NamedTuple[N2, V2])(using Tuple.Disjoint[N, N2] =:= true)
      : NamedTuple[Tuple.Concat[N, N2], Tuple.Concat[V, V2]]
      = values ++ that.values

    // inline def :* [L] (x: L): NamedTuple[Append[N, ???], Append[V, L] = ???
    // inline def *: [H] (x: H): NamedTuple[??? *: N], H *: V] = ???

    inline def map[F[_]](f: [t] => t => F[t]): NamedTuple[N, Tuple.Map[V, F]] =
      values.map(f).asInstanceOf[NamedTuple[N, Tuple.Map[V, F]]]

    inline def reverse: NamedTuple[Tuple.Reverse[N], Tuple.Reverse[V]] =
      values.reverse

    inline def zip[V2 <: Tuple](that: NamedTuple[N, V2]): NamedTuple[N, Tuple.Zip[V, V2]] =
      values.zip(that.values)

    inline def toList: List[Tuple.Union[V]] = values.toList.asInstanceOf[List[Tuple.Union[V]]]
    inline def toArray: Array[Object] = values.toArray
    inline def toIArray: IArray[Object] = values.toIArray

  end extension

  /** The names of the named tuple type `NT` */
  type Names[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[n, _] => n

  /** The value types of the named tuple type `NT` */
  type DropNames[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[_, x] => x

  type Size[X <: AnyNamedTuple] = Tuple.Size[DropNames[X]]

  type Elem[X <: AnyNamedTuple, N <: Int] = Tuple.Elem[DropNames[X], N]

  type Head[X <: AnyNamedTuple] = Elem[X, 0]

  type Last[X <: AnyNamedTuple] = Tuple.Last[DropNames[X]]

  type Init[X <: AnyNamedTuple] =
    NamedTuple[Tuple.Init[Names[X]], Tuple.Init[DropNames[X]]]

  type Tail[X <: AnyNamedTuple] = Drop[X, 1]

  type Take[X <: AnyNamedTuple, N <: Int] =
    NamedTuple[Tuple.Take[Names[X], N], Tuple.Take[DropNames[X], N]]

  type Drop[X <: AnyNamedTuple, N <: Int] =
    NamedTuple[Tuple.Drop[Names[X], N], Tuple.Drop[DropNames[X], N]]

  type Split[X <: AnyNamedTuple, N <: Int] = (Take[X, N], Drop[X, N])

  type Concat[X <: AnyNamedTuple, Y <: AnyNamedTuple] =
    NamedTuple[Tuple.Concat[Names[X], Names[Y]], Tuple.Concat[DropNames[X], DropNames[Y]]]

  type Map[X <: AnyNamedTuple, F[_ <: Tuple.Union[DropNames[X]]]] =
    NamedTuple[Names[X], Tuple.Map[DropNames[X], F]]

  type Reverse[X <: AnyNamedTuple] =
    NamedTuple[Tuple.Reverse[Names[X]], Tuple.Reverse[DropNames[X]]]

  type Zip[X <: AnyNamedTuple, Y <: AnyNamedTuple] =
    Tuple.Conforms[Names[X], Names[Y]] match
      case true =>
        NamedTuple[Names[X], Tuple.Zip[DropNames[X], DropNames[Y]]]

end NamedTuple
