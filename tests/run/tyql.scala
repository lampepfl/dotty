import language.experimental.namedTuples
import NamedTuple.{NamedTuple, AnyNamedTuple}

/* This is a demonstrator that shows how to map regular for expressions to
 * internal data that can be optimized by a query engine. It needs NamedTuples
 * and type classes but no macros. It's so far very provisional and experimental,
 * intended as a basis for further exploration.
 */

/** The type of expressions in the query language */
trait Expr[Result] extends Selectable:

  /** This type is used to support selection with any of the field names
   *  defined by Fields.
   */
  type Fields = NamedTuple.Map[NamedTuple.From[Result], Expr]

  /** A selection of a field name defined by Fields is implemented by `selectDynamic`.
   *  The implementation will add a cast to the right Expr type corresponding
   *  to the field type.
   */
  def selectDynamic(fieldName: String) = Expr.Select(this, fieldName)

  /** Member methods to implement universal equality on Expr level. */
  def == (other: Expr[?]): Expr[Boolean] = Expr.Eq(this, other)
  def != (other: Expr[?]): Expr[Boolean] = Expr.Ne(this, other)

object Expr:

  /** Sample extension methods for individual types */
  extension (x: Expr[Int])
    def > (y: Expr[Int]): Expr[Boolean] = Gt(x, y)
    def > (y: Int): Expr[Boolean] = Gt(x, IntLit(y))
  extension (x: Expr[Boolean])
    def &&(y: Expr[Boolean]): Expr[Boolean] = And(x, y)
    def || (y: Expr[Boolean]): Expr[Boolean] = Or(x, y)

  // Note: All field names of constructors in the query language are prefixed with `$`
  // so that we don't accidentally pick a field name of a constructor class where we want
  // a name in the domain model instead.

  // Some sample constructors for Exprs
  case class Gt($x: Expr[Int], $y: Expr[Int]) extends Expr[Boolean]
  case class Plus(x: Expr[Int], y: Expr[Int]) extends Expr[Int]
  case class And($x: Expr[Boolean], $y: Expr[Boolean]) extends Expr[Boolean]
  case class Or($x: Expr[Boolean], $y: Expr[Boolean]) extends Expr[Boolean]

  // So far Select is weakly typed, so `selectDynamic` is easy to implement.
  // Todo: Make it strongly typed like the other cases
  case class Select[A]($x: Expr[A], $name: String) extends Expr

  case class Single[S <: String, A]($x: Expr[A])
  extends Expr[NamedTuple[S *: EmptyTuple, A *: EmptyTuple]]

  case class Concat[A <: AnyNamedTuple, B <: AnyNamedTuple]($x: Expr[A], $y: Expr[B])
  extends Expr[NamedTuple.Concat[A, B]]

  case class Join[A <: AnyNamedTuple](a: A)
  extends Expr[NamedTuple.Map[A, StripExpr]]

  type StripExpr[E] = E match
    case Expr[b] => b

  // Also weakly typed in the arguents since these two classes model universal equality */
  case class Eq($x: Expr[?], $y: Expr[?]) extends Expr[Boolean]
  case class Ne($x: Expr[?], $y: Expr[?]) extends Expr[Boolean]

  /** References are placeholders for parameters */
  private var refCount = 0

  case class Ref[A]($name: String = "") extends Expr[A]:
    val id = refCount
    refCount += 1
    override def toString = s"ref$id(${$name})"

  /** Literals are type-specific, tailored to the types that the DB supports */
  case class IntLit($value: Int) extends Expr[Int]

  /** Scala values can be lifted into literals by conversions */
  given Conversion[Int, IntLit] = IntLit(_)

  /** The internal representation of a function `A => B`
   *  Query languages are ususally first-order, so Fun is not an Expr
   */
  case class Fun[A, B](param: Ref[A], f: B)

  type Pred[A] = Fun[A, Expr[Boolean]]

  /** Explicit conversion from
   *      (name_1: Expr[T_1], ..., name_n: Expr[T_n])
   *  to
   *      Expr[(name_1: T_1, ..., name_n: T_n)]
   */
  extension [A <: AnyNamedTuple](x: A) def toRow: Join[A] = Join(x)

  /** Same as _.toRow, as an implicit conversion */
  given [A <: AnyNamedTuple]: Conversion[A, Expr.Join[A]] = Expr.Join(_)

end Expr

/** The type of database queries. So far, we have queries
 *  that represent whole DB tables and queries that reify
 *  for-expressions as data.
 */
trait Query[A]

object Query:
  import Expr.{Pred, Fun, Ref}

  case class Filter[A]($q: Query[A], $p: Pred[A]) extends Query[A]
  case class Map[A, B]($q: Query[A], $f: Fun[A, Expr[B]]) extends Query[B]
  case class FlatMap[A, B]($q: Query[A], $f: Fun[A, Query[B]]) extends Query[B]

  // Extension methods to support for-expression syntax for queries
  extension [R](x: Query[R])

    def withFilter(p: Ref[R] => Expr[Boolean]): Query[R] =
      val ref = Ref[R]()
      Filter(x, Fun(ref, p(ref)))

    def map[B](f: Ref[R] => Expr[B]): Query[B] =
      val ref = Ref[R]()
      Map(x, Fun(ref, f(ref)))

    def flatMap[B](f: Ref[R] => Query[B]): Query[B] =
      val ref = Ref[R]()
      FlatMap(x, Fun(ref, f(ref)))
end Query

/** The type of query references to database tables */
case class Table[R]($name: String) extends Query[R]

// Everything below is code using the model -----------------------------

// Some sample types
case class City(zipCode: Int, name: String, population: Int)
type Address = (city: City, street: String, number: Int)
type Person = (name: String, age: Int, addr: Address)

@main def Test =

  val cities = Table[City]("cities")

  val q1 = cities.map: c =>
    c.zipCode
  val q2 = cities.withFilter: city =>
      city.population > 10_000
    .map: city =>
      city.name

  val q3 =
    for
      city <- cities
      if city.population > 10_000
    yield city.name

  val q4 =
    for
      city <- cities
      alt <- cities
      if city.name == alt.name && city.zipCode != alt.zipCode
    yield
      city

  val addresses = Table[Address]("addresses")
  val q5 =
    for
      city <- cities
      addr <- addresses
      if addr.street == city.name
    yield
      (name = city.name, num = addr.number)

  val q6 =
    cities.map: city =>
      (name = city.name, zipCode = city.zipCode)

  def run[T](q: Query[T]): Iterator[T] = ???

  def x1: Iterator[Int] = run(q1)
  def x2: Iterator[String] = run(q2)
  def x3: Iterator[String] = run(q3)
  def x4: Iterator[City] = run(q4)
  def x5: Iterator[(name: String, num: Int)] = run(q5)
  def x6: Iterator[(name: String, zipCode: Int)] = run(q6)

  println(q1)
  println(q2)
  println(q3)
  println(q4)
  println(q5)
  println(q6)

/* The following is not needed currently

/** A type class for types that can map to a database table */
trait Row:
  type Self
  type Fields = NamedTuple.From[Self]
  type FieldExprs = NamedTuple.Map[Fields, Expr]

  //def toFields(x: Self): Fields = ???
  //def fromFields(x: Fields): Self = ???

*/