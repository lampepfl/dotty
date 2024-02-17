---
layout: doc-page
title: "Named Tuples"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/named-tuples.html
---

The elements of a tuple can now be named. Example:
```scala
type Person = (name: String, age: Int)
val Bob: Person = (name = "Bob", age = 33)

Bob match
  case (name, age) =>
    println(s"$name is $age years old")

val persons: List[Person] = ...
val minors = persons.filter: p =>
  p.age < 18
```
Named bindings in tuples are similar to function parameters and arguments. We use `name: Type` for element types and `name = value` for element values. It is illegal to mix named and unnamed elements in a tuple, or to use the same same
name for two different elements.

Fields of named tuples can be selected by their name, as in the line `p.age < 18` above.

### Conformance and Convertibility

The order of names in a named tuple matters. For instance, the type `Person` above and the type `(age: Int, name: String)` would be different, incompatible types.

Values of named tuple types can also be be defined using regular tuples. For instance:
```scala
val Laura: Person = ("Laura", 25)

def register(person: Person) = ...
register(person = ("Silvain", 16))
register(("Silvain", 16))
```
This follows since a regular tuple `(T_1, ..., T_n)` is treated as a subtype of a named tuple `(N_1 = T_1, ..., N_n = T_n)` with the same element types.

In the other direction, one can convert a named tuple to an unnamed tuple with the `toTuple` method. Example:
```scala
val x: (String, Int) = Bob.toTuple // ok
```
`toTuple` is defined as an extension method in the `NamedTuple` object.
It returns the given tuple unchanged and simply "forgets" the names.

A `.toTuple` selection is inserted implicitly by the compiler if it encounters a named tuple but the expected type is a regular tuple. So the following works as well:
```scala
val x: (String, Int) = Bob  // works, expanded to Bob.toTuple
```
The difference between subtyping in one direction and automatic `.toTuple` conversions in the other is relatively minor. The main difference is that `.toTuple` conversions don't work inside type constructors. So the following is OK:
```scala
  val names = List("Laura", "Silvain")
  val ages = List(25, 16)
  val persons: List[Person] = names.zip(ages)
```
But the following would be illegal.
```scala
  val persons: List[Person] = List(Bob, Laura)
  val pairs: List[(String, Int)] = persons // error
```
We would need an explicit `_.toTuple` selection to express this:
```scala
  val pairs: List[(String, Int)] = persons.map(_.toTuple)
```
Note that conformance rules for named tuples are analogous to the rules for named parameters. One can assign parameters by position to a named parameter list.
```scala
  def f(param: Int) = ...
  f(param = 1)   // OK
  f(2)           // Also OK
```
But one cannot use a name to pass an argument to an unnamed parameter:
```scala
    val f: Int => T
    f(2)         // OK
    f(param = 2) // Not OK
```
The rules for tuples are analogous. Unnamed tuples conform to named tuple types, but the opposite requires a conversion.

### Pattern Matching

When pattern matching on a named tuple, the pattern may be named or unnamed.
If the pattern is named it needs to mention only a subset of the tuple names, and these names can come in any order. So the following are all OK:
```scala
Bob match
  case (name, age) => ...

Bob match
  case (name = x, age = y) => ...

Bob match
  case (age = x) => ...

Bob match
  case (age = x, name = y) => ...
```

### Expansion

Named tuples are in essence just a convenient syntax for regular tuples. In the internal representation, a named tuple type is represented at compile time as a pair of two tuples. One tuple contains the names as literal constant string types, the other contains the element types. The runtime representation of a named tuples consists of just the element values, whereas the names are forgotten. This is achieved  by declaring `NamedTuple`
in package `scala` as an opaque type as follows:
```scala
  opaque type NamedTuple[N <: Tuple, +V <: Tuple] >: V = V
```
For instance, the `Person` type would be represented as the type
```scala
NamedTuple[("name", "age"), (String, Int)]
```
`NamedTuple` is an opaque type alias of its second, value parameter. The first parameter is a string constant type which determines the name of the element. Since the type is just an alias of its value part, names are erased at runtime, and named tuples and regular tuples have the same representation.

A `NamedTuple[N, V]` type is publicly known to be a supertype (but not a subtype) of its value paramater `V`, which means that regular tuples can be assigned to named tuples but not _vice versa_.

The `NamedTuple` object contains a number of extension methods for named tuples hat mirror the same functions in `Tuple`. Examples are
`apply`, `head`, `tail`, `take`, `drop`, `++`, `map`, or `zip`.
Similar to `Tuple`, the `NamedTuple` object also contains types such as `Elem`, `Head`, `Concat`
that describe the results of these extension methods.

The translation of named tuples to instances of `NamedTuple` is fixed by the specification and therefore known to the programmer. This means that:

 - All tuple operations also work with named tuples "out of the box".
 - Macro libraries can rely on this expansion.

### The NamedTuple.From Type

The `NamedTuple` object contains a type definition
```scala
  type From[T] <: AnyNamedTuple
```
`From` is treated specially by the compiler. When `NamedTuple.From` is applied to
an argument type that is an instance of a case class, the type expands to the named
tuple consisting of all the fields of that case class. Here, fields means: elements of the first parameter section. For instance, assuming
```scala
case class City(zip: Int, name: String, population: Int)
```
then `NamedTuple.From[City]` is the named tuple
```scala
(zip: Int, name: String, population: Int)
```
The same works for enum cases expanding to case classes.

### Restrictions

The following restrictions apply to named tuple elements:

 1. Either all elements of a tuple are named or none are named. It is illegal to mix named and unnamed elements in a tuple. For instance, the following is in error:
    ```scala
    val illFormed1 = ("Bob", age = 33)  // error
    ```
 2. Each element name in a named tuple must be unique. For instance, the following is in error:
    ```scala
    val illFormed2 = (name = "", age = 0, name = true)  // error
    ```
 3. Named tuples can be matched with either named or regular patterns. But regular tuples and other selector types can only be matched with regular tuple patterns. For instance, the following is in error:
    ```scala
    (tuple: Tuple) match
        case (age = x) => // error
    ```

### Syntax

The syntax of Scala is extended as follows to support named tuples:
```
SimpleType        ::=  ...
                    |  ‘(’ NameAndType {‘,’ NameAndType} ‘)’
NameAndType       ::=  id ':' Type

SimpleExpr        ::=  ...
                    |  '(' NamedExprInParens {‘,’ NamedExprInParens} ')'
NamedExprInParens ::=  id '=' ExprInParens

Patterns          ::=  Pattern {‘,’ Pattern}
                    |  NamedPattern {‘,’ NamedPattern}
NamedPattern      ::=  id '=' Pattern
```

### Named Pattern Matching

We allow named patterns not just for named tuples but also for case classes.
For instance:
```scala
city match
  case c @ City(name = "London") => println(p.population)
  case City(name = n, zip = 1026, population = pop) => println(pop)
```

Named constructor patterns are analogous to named tuple patterns. In both cases

 - either all fields are named or none is,
 - every name must match the name some field of the selector,
 - names can come in any order,
 - not all fields of the selector need to be matched.

This revives SIP 43, with a much simpler desugaring than originally proposed.
Named patterns are compatible with extensible pattern matching simply because
`unapply` results can be named tuples.

### Source Incompatibilities

There are some source incompatibilities involving named tuples of length one.
First, what was previously classified as an assignment could now be interpreted as a named tuple. Example:
```scala
var age: Int
(age = 1)
```
This was an assignment in parentheses before, and is a named tuple of arity one now. It is however not idiomatic Scala code, since assignments are not usually enclosed in parentheses.

Second, what was a named argument to an infix operator can now be interpreted as a named tuple.
```scala
class C:
  infix def f(age: Int)
val c: C
```
then
```scala
c f (age = 1)
```
will now construct a tuple as second operand instead of passing a named parameter.

### Computed Field Names

The `Selectable` trait now has a `Fields` type member that can be instantiated
to a named tuple.

```scala
trait Selectable:
  type Fields <: NamedTuple.AnyNamedTuple
```

If `Fields` is instantiated in a subclass of `Selectable` to some named tuple type,
then the available fields and their types will be defined by that type. Assume `n: T`
is an element of the `Fields` type in some class `C` that implements `Selectable`,
that `c: C`, and that `n` is not otherwise legal as a name of a selection on `c`.
Then `c.n` is a legal selection, which expands to `c.selectDynamic("n").asInstanceOf[T]`.

It is the task of the implementation of `selectDynamic` in `C` to ensure that its
computed result conforms to the predicted type `T`

As an example, assume we have a query type `Q[T]` defined as follows:

```scala
trait Q[T] extends Selectable:
  type Fields = NamedTuple.Map[NamedTuple.From[T], Q]
  def selectDynamic(fieldName: String) = ...
```

Assume in the user domain:
```scala
case class City(zipCode: Int, name: String, population: Int)
val city: Q[City]
```
Then
```scala
city.zipCode
```
has type `Q[Int]` and it expands to
```scala
city.selectDynamic("zipCode").asInstanceOf[Q[Int]]
```
