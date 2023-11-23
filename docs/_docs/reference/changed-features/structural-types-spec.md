---
layout: doc-page
title: "Programmatic Structural Types - More Details"
nightlyOf: https://docs.scala-lang.org/scala3/reference/changed-features/structural-types-spec.html
---

## Syntax

```ebnf
SimpleType    ::= ... | Refinement
Refinement    ::= ‘{’ RefineStatSeq ‘}’
RefineStatSeq ::=  RefineStat {semi RefineStat}
RefineStat    ::= ‘val’ VarDcl | ‘def’ DefDcl | ‘type’ {nl} TypeDcl
```

## Implementation of Structural Types

The standard library defines a universal marker trait
[`scala.Selectable`](https://github.com/lampepfl/dotty/blob/main/library/src/scala/Selectable.scala):

```scala
trait Selectable extends Any
```

An implementation of `Selectable` that relies on [Java reflection](https://www.oracle.com/technical-resources/articles/java/javareflection.html) is
available in the standard library: `scala.reflect.Selectable`. Other
implementations can be envisioned for platforms where Java reflection
is not available.

Implementations of `Selectable` have to make available one or both of
the methods `selectDynamic` and `applyDynamic`. The methods could be members of the `Selectable` implementation or they could be extension methods.

The `selectDynamic` method takes a field name and returns the value associated with that name in the `Selectable`.
It should have a signature of the form:

```scala
def selectDynamic(name: String): T
```

Often, the return type `T` is `Any`.

Unlike `scala.Dynamic`, there is no special meaning for an `updateDynamic` method.
However, we reserve the right to give it meaning in the future.
Consequently, it is recommended not to define any member called `updateDynamic` in `Selectable`s.

The `applyDynamic` method is used for selections that are applied to arguments. It takes a method name and possibly `Class`es representing its parameters types as well as the arguments to pass to the function.
Its signature should be of one of the two following forms:

```scala
def applyDynamic(name: String)(args: Any*): T
def applyDynamic(name: String, ctags: Class[?]*)(args: Any*): T
```

Both versions are passed the actual arguments in the `args` parameter. The second version takes in addition a vararg argument of `java.lang.Class`es that identify the method's parameter classes. Such an argument is needed
if `applyDynamic` is implemented using Java reflection, but it could be
useful in other cases as well. `selectDynamic` and `applyDynamic` can also take additional context parameters in using clauses. These are resolved in the normal way at the callsite.

Typically, vararg arguments of a function at callsite are represented as an `Array(elems)`. But in case of `scala.Selectable` implementation relied on Java reflection `scala.reflect.Selectable` when vararg arguments passed to `applyDynamic` method, they get interpreted as single parameter instead of multiple.
To overcome this they should be wrapped in `Seq(elems)` to be considered as multiple parameters at callsite of `scala.reflect.Selectable.applyDynamic`.
```scala
class Reflective extends reflect.Selectable
class Foo(val i: Int) extends AnyVal
val reflective = new Reflective  {
  def varargs(x: Int, foo: Foo*) = foo.map(_.i).sum + x
}
val varargs = List(Foo(1), Foo(2), Foo(3))
reflective.varargs(1, varargs:_*)
```

Given a value `v` of type `C { Rs }`, where `C` is a class reference
and `Rs` are structural refinement declarations, and given `v.a` of type `U`, we consider three distinct cases:

- If `U` is a value type, we map `v.a` to:
  ```scala
  v.selectDynamic("a").asInstanceOf[U]
  ```

- If `U` is a method type `(T11, ..., T1n)...(TN1, ..., TNn): R` and it is not a dependent method type, we map `v.a(a11, ..., a1n)...(aN1, ..., aNn)` to:
  ```scala
  v.applyDynamic("a")(a11, ..., a1n, ..., aN1, ..., aNn)
    .asInstanceOf[R]
  ```
  If this call resolves to an `applyDynamic` method of the second form that takes a `Class[?]*` argument, we further rewrite this call to
  ```scala
  v.applyDynamic("a", c11, ..., c1n, ..., cN1, ... cNn)(
    a11, ..., a1n, ..., aN1, ..., aNn)
    .asInstanceOf[R]
  ```
   where each `c_ij` is the literal `java.lang.Class[?]` of the type of the formal parameter `Tij`, i.e., `classOf[Tij]`.

- If `U` is neither a value nor a method type, or a dependent method
  type, an error is emitted.

Note that `v`'s static type does not necessarily have to conform to `Selectable`, nor does it need to have `selectDynamic` and `applyDynamic` as members. It suffices that there is an implicit
conversion that can turn `v` into a `Selectable`, and the selection methods could also be available as
[extension methods](../contextual/extension-methods.md).

## Limitations of Structural Types

- Dependent methods cannot be called via structural call.

- Refinements may not introduce overloads: If a refinement specifies the signature
  of a method `m`, and `m` is also defined in the parent type of the refinement, then
  the new signature must properly override the existing one.

- Subtyping of structural refinements must preserve erased parameter types: Assume
  we want to prove `S <: T { def m(x: A): B }`. Then, as usual, `S` must have a member method `m` that can take an argument of type `A`. Furthermore, if `m` is not a member of `T` (i.e. the refinement is structural), an additional condition applies. In this case, the member _definition_ `m` of `S` will have a parameter
  with type `A'` say. The additional condition is that the erasure of `A'` and `A` is the same. Here is an example:

  ```scala
  class Sink[A] { def put(x: A): Unit = {} }
  val a = Sink[String]()
  val b: { def put(x: String): Unit } = a  // error
  b.put("abc") // looks for a method with a `String` parameter
  ```
  The second to last line is not well-typed,
  since the erasure of the parameter type of `put` in class `Sink` is `Object`,
  but the erasure of `put`'s parameter in the type of `b` is `String`.
  This additional condition is necessary, since we will have to resort
  to some (as yet unknown) form of reflection to call a structural member
  like `put` in the type of `b` above. The condition ensures that the statically
  known parameter types of the refinement correspond up to erasure to the
  parameter types of the selected call target at runtime.

  Most reflection dispatch algorithms need to know exact erased parameter types. For instance, if the example above would typecheck, the call
  `b.put("abc")` on the last line would look for a method `put` in the runtime type of `b` that takes a `String` parameter. But the `put` method is the one from class `Sink`, which takes an `Object` parameter. Hence the call would fail at runtime with a `NoSuchMethodException`.

  One might hope for a "more intelligent" reflexive dispatch algorithm that does not require exact parameter type matching. Unfortunately, this can always run into ambiguities, as long as overloading is a possibility. For instance, continuing the example above, we might introduce a new subclass `Sink1` of `Sink` and change the definition of `a` as follows:

  ```scala
  class Sink1[A] extends Sink[A] { def put(x: "123") = ??? }
  val a: Sink[String] = Sink1[String]()
  ```

  Now there are two `put` methods in the runtime type of `b` with erased parameter
  types `Object` and `String`, respectively. Yet dynamic dispatch still needs to go
  to the first `put` method, even though the second looks like a better match.

  For the cases where we can in fact implement reflection without knowing precise parameter types (for instance if static overloading is replaced by dynamically dispatched multi-methods), there is an escape hatch. For types that extend `scala.Selectable.WithoutPreciseParameterTypes` the signature check is omitted. Example:

  ```scala
  trait MultiMethodSelectable extends Selectable.WithoutPreciseParameterTypes:
    // Assume this version of `applyDynamic` can be implemented without knowing
    // precise parameter types `paramTypes`:
    def applyDynamic(name: String, paramTypes: Class[_]*)(args: Any*): Any = ???

  class Sink[A] extends MultiMethodSelectable:
    def put(x: A): Unit = {}

  val a = new Sink[String]
  val b: MultiMethodSelectable { def put(x: String): Unit } = a  // OK
  ```
## Differences with Scala 2 Structural Types

- Scala 2 supports structural types by means of Java reflection. Unlike
  Scala 3, structural calls do not rely on a mechanism such as
  `Selectable`, and reflection cannot be avoided.
- In Scala 2, refinements can introduce overloads.
- In Scala 2, mutable `var`s are allowed in refinements. In Scala 3,
  they are no longer allowed.
- Scala 2 does not impose the "same-erasure" restriction on subtyping of structural types. It allows some calls to fail at runtime instead.

## Context

For more information, see [Rethink Structural Types](https://github.com/lampepfl/dotty/issues/1886).
