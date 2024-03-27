---
layout: doc-page
title: "Modularity Improvements"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/modularity.html
---

# Modularity Improvements

Martin Odersky, 7.1.2024

Scala is a language in the SML tradition, in the sense that it has
abstract and alias types as members of modules (which in Scala take the form of objects and classes). This leads to a simple dependently
typed system, where dependencies in types are on paths instead of full terms.

So far, some key ingredients were lacking which meant that module composition with functors is harder in Scala than in SML. In particular, one often needs to resort the infamous `Aux` pattern that lifts type members into type parameters so that they can be tracked across class instantiations. This makes modular, dependently typed programs
much harder to write and read, and makes such programming only accessible to experts.

In this note I propose some small changes to Scala's dependent typing that makes
modular programming much more straightforward.

The suggested improvements have been implemented and are available
in source version `future` if the additional experimental language import `modularity` is present. For instance, using the following command:

```
  scala compile -source:future -language:experimental.modularity
```

## Tracked Parameters

Scala is dependently typed for functions, but unfortunately not for classes.
For instance, consider the following definitions:

```scala
  class C:
    type T
    ...

  def f(x: C): x.T = ...

  val y: C { type T = Int }
```
Then `f(y)` would have type `Int`, since the compiler will substitute the
concrete parameter reference `y` for the formal parameter `x` in the result
type of `f`, and `y.T = Int`

However, if we use a class `F` instead of a method `f`, things go wrong.

```scala
  class F(val x: C):
    val result: x.T = ...
```
Now `F(y).result` would not have type `Int` but instead the rather less useful type `?1.T` where `?1` is a so-called skolem constant of type `C` (a skolem represents an unknown value).

This shortcoming means that classes cannot really be used for advanced
modularity constructs that rely on dependent typing.

**Proposal:** Introduce a `tracked` modifier that can be added to
a `val` parameter of a class or trait. For every tracked class parameter of a class `C`, add a refinement in the constructor type of `C` that the class member is the same as the parameter.

**Example:** In the setting above, assume `F` is instead declared like this:
```scala
  class F(tracked val x: C):
    val result: x.T = ...
```
Then the constructor `F` would get roughly the following type:
```scala
  F(x1: C): F { val x: x1.type }
```
_Aside:_ More precisely, both parameter and refinement would apply to the same name `x` but the refinement still refers to the parameter. We unfortunately can't express that in source, however, so we chose the new name `x1` for the parameter in the explanation.

With the new constructor type, the expression `F(y).result` would now have the type `Int`, as hoped for. The reasoning to get there is as follows:

 - The result of the constructor `F(y)` has type `F { val x: y.type }` by
   the standard typing for dependent functions.
 - The type of `result` inside `F` is `x.T`.
 - Hence, the type of `result` as a member of `F { val x: y.type }` is `y.T`, which is equal to `Int`.

The addition of tracked parameters makes classes suitable as a fundamental modularity construct supporting dependent typing. Here is an example, taken from issue #3920:

```scala
trait Ordering:
  type T
  def compare(t1:T, t2: T): Int

class SetFunctor(tracked val ord: Ordering):
  type Set = List[ord.T]

  def empty: Set = Nil

  extension (s: Set)
    def add(x: ord.T): Set = x :: remove(x)
    def remove(x: ord.T): Set = s.filter(e => ord.compare(x, e) != 0)
    def contains(x: ord.T): Boolean = s.exists(e => ord.compare(x, e) == 0)

object intOrdering extends Ordering:
  type T = Int
  def compare(t1: T, t2: T): Int = t1 - t2

val IntSet = new SetFunctor(intOrdering)

@main def Test =
  import IntSet.*
  val set = IntSet.empty.add(6).add(8).add(23)
  assert(!set.contains(7))
  assert(set.contains(8))
```
This works as it should now. Without the addition of `tracked` to the
parameter of `SetFunctor` typechecking would immediately lose track of
the element type `T` after an `add`, and would therefore fail.

**Syntax Change**

```
ClsParam  ::=  {Annotation} [{Modifier | ‘tracked’} (‘val’ | ‘var’) | ‘inline’] Param
```

The (soft) `tracked` modifier is only allowed for `val` parameters of classes.

**Discussion**

Since `tracked` is so useful, why not assume it by default? First, `tracked` makes sense only for `val` parameters. If a class parameter is not also a field declared using `val` then there's nothing to refine in the constructor result type. One could think of at least making all `val` parameters tracked by default, but that would be a backwards incompatible change. For instance, the following code would break:

```scala
case class Foo(x: Int)
var foo = Foo(1)
if someCondition then foo = Foo(2)
```
If we assume `tracked` for parameter `x` (which is implicitly a `val`),
then `foo` would get inferred type `Foo { val x: 1 }`, so it could not
be reassigned to a value of type `Foo { val x: 2 }` on the next line.

Another approach might be to assume `tracked` for a `val` parameter `x`
only if the class refers to a type member of `x`. But it turns out that this
scheme is unimplementable since it would quickly lead to cyclic references
when typechecking recursive class graphs. So an explicit `tracked` looks like the best available option.

## Allow Class Parents to be Refined Types

Since `tracked` parameters create refinements in constructor types,
it is now possible that a class has a parent that is a refined type.
Previously such types were not permitted, since we were not quite sure how to handle them. But with tracked parameters it becomes pressing so
admit such types.

**Proposal** Allow refined types as parent types of classes. All refinements that are inherited in this way become synthetic members of the class.

**Example**

```scala
class C:
  type T
  def m(): T

type R = C:
  type T = Int
  def m(): 22

class D extends R:
  def next(): D
```
This code now compiles. The definition of `D` is expanded as follows:

```scala
class D extends C:
  def next(): D
  /*synthetic*/ type T = Int
  /*synthetic*/ def m(): 22
```
Note how class refinements are moved from the parent constructor of `D` into the body of class `D` itself.

This change does not entail a syntax change. Syntactically, parent types cannot be refined types themselves. So the following would be illegal:
```scala
class D extends C { type T = Int; def m(): 22 }: // error
  def next(): D
```
If a refined type should be used directly as a parent type of a class, it needs to come in parentheses:
```scala
class D extends (C { type T = Int; def m(): 22 }) // ok
  def next(): D
```

## A Small Relaxation To Export Rules

The rules for export forwarders are changed as follows.

Previously, all export forwarders were declared `final`. Now, only term members are declared `final`. Type aliases are left aside.

This makes it possible to export the same type member into several traits and then mix these traits in the same class. The test file `tests/pos/typeclass-aggregates.scala` shows why this is essential if we want to combine multiple givens with type members in a new given that aggregates all these givens in an intersection type.

The change does not lose safety since different type aliases would in any case lead to uninstantiatable classes.