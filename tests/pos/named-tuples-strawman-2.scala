import compiletime.*
import compiletime.ops.int.*
import compiletime.ops.boolean.!
import Tuple.*

object TupleOps:

  /** The `X` tuple, with its element at index `N` replaced by `Y`.
   *  If `N` is equal to `Size[X]`, the element `Y` is appended instead
   */
  type UpdateOrAppend[X <: Tuple, N <: Int, Y] <: Tuple = X match
    case x *: xs =>
      N match
        case 0 => Y *: xs
        case S[n1] => x *: UpdateOrAppend[xs, n1, Y]
    case EmptyTuple =>
      N match
        case 0 => Y *: EmptyTuple

  inline def updateOrAppend[X <: Tuple, N <: Int, Y](xs: X, y: Y): UpdateOrAppend[X, N, Y] =
    locally:
      val n = constValue[N]
      val size = xs.size
      require(0 <= n && n <= xs.size, s"Index $n out of range 0..$size")
      if n == size then xs :* y
      else
        val elems = xs.toArray
        elems(n) = y.asInstanceOf[Object]
        fromArray(elems)
    .asInstanceOf[UpdateOrAppend[X, N, Y]]

  extension [X <: Tuple](inline xs: X)
    // Note: Y must be inferred precisely, or given explicitly. This means even though `updateOrAppend`
    // is clearly useful, we cannot yet move it to tuple since it is still too awkward to use.
    // Once we have precise inference, we could replace `Y <: Singleton` with `Y: Precise`
    // and then it should work beautifully.
    inline def updateOrAppend[N <: Int & Singleton, Y <: Singleton](inline n: N, inline y: Y): UpdateOrAppend[X, N, Y] =
      locally:
        val size = xs.size
        require(0 <= n && n <= size, s"Index $n out of range 0..$size")
        if n == size then xs :* y
        else
          val elems = xs.toArray
          elems(n) = y.asInstanceOf[Object]
          fromArray(elems)
      .asInstanceOf[UpdateOrAppend[X, N, Y]]

  /** If `Y` does not occur in tuple `X`, `X` with `Y` appended. Otherwise `X`. */
  type AppendIfDistinct[X <: Tuple, Y] <: Tuple = X match
    case Y *: xs => X
    case x *: xs => x *: AppendIfDistinct[xs, Y]
    case EmptyTuple => Y *: EmptyTuple

  inline def appendIfDistinct[X <: Tuple, Y](xs: X, y: Y): AppendIfDistinct[X, Y] =
    (if xs.containsType[Y] then xs else xs :* y).asInstanceOf[AppendIfDistinct[X, Y]]

  /** `X` with all elements from `Y` that do not occur in `X` appended */
  type ConcatDistinct[X <: Tuple, Y <: Tuple] <: Tuple = Y match
    case y *: ys => ConcatDistinct[AppendIfDistinct[X, y], ys]
    case EmptyTuple => X

  inline def concatDistinct[X <: Tuple, Y <: Tuple](xs: X, ys: Y): ConcatDistinct[X, Y] =
    (xs ++ ys.filter[Y, [Elem] =>> ![Contains[X, Elem]]]).asInstanceOf[ConcatDistinct[X, Y]]

object NamedTupleDecomposition:
  import NamedTupleOps.*

  /** The names of the named tuple type `NT` */
  type Names[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[n, _] => n

  /** The value types of the named tuple type `NT` */
  type DropNames[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[_, x] => x

object NamedTupleOps:
  import TupleOps.*

  opaque type AnyNamedTuple = Any

  opaque type NamedTuple[N <: Tuple, +X <: Tuple] >: X <: AnyNamedTuple = X

  export NamedTupleDecomposition.*

  object NamedTuple:
    def apply[N <: Tuple, X <: Tuple](x: X): NamedTuple[N, X] = x

  extension [NT <: AnyNamedTuple](x: NT)
    inline def toTuple: DropNames[NT] = x.asInstanceOf
    inline def names: Names[NT] = constValueTuple[Names[NT]]

  /** Internal use only: Merge names and value components of two named tuple to
   *  impement `UpdateWith`.
   *  @param N  the names of the combined tuple
   *  @param X  the value types of the first named tuple
   *  @param N2 the names of the second named tuple
   *  @param Y  the value types of the second named tuple
   */
  type Merge[N <: Tuple, X <: Tuple, N2 <: Tuple, Y <: Tuple] = (N2, Y) match
    case (n *: ns, y *: ys) =>
      Merge[N, UpdateOrAppend[X, IndexOf[N, n], y], ns, ys]
    case (EmptyTuple, EmptyTuple) =>
      NamedTuple[N, X]

  /** A joint named tuple where
   *   - The names are the names of named tuple `NT1` followed by those names of `NT2` which
   *     do not appear in `NT1`
   *   - The values are the values of `NT1` and `NT2` corresponding to these names.
   *     If a name is present in both `NT1` and `NT2` the value in `NT2` is used.
   */
  type UpdateWith[NT1 <: AnyNamedTuple, NT2 <: AnyNamedTuple] =
    Merge[ConcatDistinct[Names[NT1], Names[NT2]], DropNames[NT1], Names[NT2], DropNames[NT2]]

  extension [NT1 <: AnyNamedTuple](nt1: NT1)
    inline def updateWith[NT2 <: AnyNamedTuple](nt2: NT2): UpdateWith[NT1, NT2] =
      val names = constValueTuple[ConcatDistinct[Names[NT1], Names[NT2]]].toArray
      val names2 = constValueTuple[Names[NT2]].toArray
      val values1 = nt1.toTuple
      val values2 = nt2.toTuple
      val values = new Array[Object](names.length)
      values1.toArray.copyToArray(values)
      for i <- 0 until values2.size do
        val idx = names.indexOf(names2(i))
        values(idx) = values2.productElement(i).asInstanceOf[Object]
      Tuple.fromArray(values).asInstanceOf[UpdateWith[NT1, NT2]]

@main def Test =
  import TupleOps.*
  import NamedTupleOps.*

  type Names = "first" *: "last" *: "age" *: EmptyTuple
  type Values = "Bob" *: "Miller" *: 33 *: EmptyTuple

  val names: Names = ("first", "last", "age")
  val values: Values = ("Bob", "Miller", 33)

  val x1: IndexOf[Names, "first"] = constValue
  val _: 0 = x1

  val x2: IndexOf[Names, "age"] = names.indexOfType["age"]
  val _: 2 = x2

  val x3: IndexOf[Names, "what?"] = names.indexOfType["what?"]
  val _: 3 = x3

  type Releases = "first" *: "middle" *: EmptyTuple
  type ReleaseValues = 1.0 *: true *: EmptyTuple

  val releases: Releases = ("first", "middle")
  val releaseValues: ReleaseValues = (1.0, true)

  val x4 = values.updateOrAppend(names.indexOfType["age"], 11)
    //updateOrAppend[Values](values)[IndexOf[Names, "age"], 11](indexOf[Names](names)["age"]("age"), 11)
  val _: ("Bob", "Miller", 11) = x4
  assert(("Bob", "Miller", 11) == x4)

  val x5 = updateOrAppend[Values, IndexOf[Names, "what"], true](values, true)
  val _: ("Bob", "Miller", 33, true) = x5
  assert(("Bob", "Miller", 33, true) == x5)

  val x6 = updateOrAppend[Values, IndexOf[Names, "first"], "Peter"](values, "Peter")
  val _: ("Peter", "Miller", 33) = x6
  assert(("Peter", "Miller", 33) == x6)

  val x7 = concatDistinct[Names, Releases](names, releases)
  val _: ("first", "last", "age", "middle") = x7
  assert(("first", "last", "age", "middle") == x7, x7)

  val x8 = concatDistinct[Releases, Names](releases, names)
  val _: ("first", "middle", "last", "age") = x8
  assert(("first", "middle", "last", "age") == x8)

  def x9: Merge[ConcatDistinct[Names, Releases], Values, Releases, ReleaseValues] = ???
  def x9c: NamedTuple[("first", "last", "age", "middle"), (1.0, "Miller", 33, true)] = x9

  val person = NamedTuple[Names, Values](values)
  val release = NamedTuple[Releases, ReleaseValues](releaseValues)

  val x10 = person.updateWith(release)
  val _: UpdateWith[NamedTuple[Names, Values], NamedTuple[Releases, ReleaseValues]] = x10
  val _: ("first", "last", "age", "middle") = x10.names
  val _: (1.0, "Miller", 33, true) = x10.toTuple
  assert((("first", "last", "age", "middle") == x10.names))
  assert((1.0, "Miller", 33, true) == x10.toTuple)

  val x11 = release.updateWith(person)
  val _: UpdateWith[NamedTuple[Releases, ReleaseValues], NamedTuple[Names, Values]] = x11
  val _: NamedTuple[("first", "middle", "last", "age"), ("Bob", true, "Miller", 33)] = x11
  assert(("first", "middle", "last", "age") == x11.names)
  assert(("Bob", true, "Miller", 33) == x11.toTuple)
