---
layout: doc-page
title: "Changes in Implicit Resolution"
nightlyOf: https://docs.scala-lang.org/scala3/reference/changed-features/implicit-resolution.html
---

This section describes changes to the implicit resolution that apply both to the new `given`s and to the old-style `implicit`s in Scala 3.
Implicit resolution uses a new algorithm which caches implicit results
more aggressively for performance. There are also some changes that
affect implicits on the language level.

**1.** Types of implicit values and result types of implicit methods
must be explicitly declared. Excepted are only values in local blocks
where the type may still be inferred:
```scala
  class C {

    val ctx: Context = ...        // ok

    /*!*/ implicit val x = ...    // error: type must be given explicitly

    /*!*/ implicit def y = ...    // error: type must be given explicitly
  }
  val y = {
    implicit val ctx = this.ctx // ok
    ...
  }
```
**2.** Nesting is now taken into account for selecting an implicit. Consider for instance the following scenario:
```scala
  def f(implicit i: C) = {
    def g(implicit j: C) = {
      implicitly[C]
    }
  }
```
This will now resolve the `implicitly` call to `j`, because `j` is nested
more deeply than `i`. Previously, this would have resulted in an
ambiguity error. The previous possibility of an implicit search failure
due to _shadowing_ (where an implicit is hidden by a nested definition)
no longer applies.

**3.** Package prefixes no longer contribute to the implicit search scope of a type. Example:
```scala
  package p

  given a: A = A()

  object o:
    given b: B = B()
    type C
```
Both `a` and `b` are visible as implicits at the point of the definition
of `type C`. However, a reference to `p.o.C` outside of package `p` will
have only `b` in its implicit search scope but not `a`.

In more detail, here are the rules for what constitutes the implicit scope of
a type:

**Definition:** A reference is an _anchor_ if it refers to an object, a class, a trait, an abstract type, an opaque type alias, or a match type alias. References to packages and package objects are anchors only under `-source:3.0-migration`.
Opaque type aliases count as anchors only outside the scope where their alias is visible.

**Definition:** The _anchors_ of a type _T_ is a set of references defined as follows:

  1. If _T_ is a reference to an anchor, _T_ itself plus, if _T_ is of the form _P#A_, the anchors of _P_.
  1. If _T_ is an alias of _U_, the anchors of _U_.
  1. If _T_ is a reference to a type parameter, the union of the anchors of both of its bounds.
  1. If _T_ is a singleton reference, the anchors of its underlying type, plus,
    if _T_ is of the form _(P#x).type_, the anchors of _P_.
  1. If _T_ is the this-type _o.this_ of a static object _o_, the anchors of a term reference _o.type_ to that object,
  1. If _T_ is some other this-type _P.this.type_, the anchors of _P_.
  1. If _T_ is some other type, the union of the anchors of each constituent type of _T_.

 **Definition:** The _implicit scope_ of a type _T_ is the smallest set _S_ of term references such that

  1. If _T_ is a reference to a class, _S_ includes a reference to the companion object
    of the class, if it exists, as well as the implicit scopes of all of _T_'s parent classes.
  1. If _T_ is a reference to an object, _S_ includes _T_ itself as well as
    the implicit scopes of all of _T_'s parent classes.
  1. If _T_ is a reference to an opaque type alias named _A_, _S_ includes
    a reference to an object _A_ defined in the same scope as the type, if it exists,
    as well as the implicit scope of _T_'s underlying type or bounds.
  1. If _T_ is a reference to an abstract type or match type alias
    named _A_, _S_ includes a reference to an object _A_ defined in the same scope as the type, if it exists, as well as the implicit scopes of _T_'s given bounds.
  1. If _T_ is a reference to an anchor of the form _p.A_ then _S_ also includes
    all term references on the path _p_.
  1. If _T_ is some other type, _S_ includes the implicit scopes of all anchors of _T_.


**4.** The treatment of ambiguity errors has changed. If an ambiguity is encountered in some recursive step of an implicit search, the ambiguity is propagated to the caller.

Example: Say you have the following definitions:
```scala
  class A
  class B extends C
  class C
  implicit def a1: A
  implicit def a2: A
  implicit def b(implicit a: A): B
  implicit def c: C
```
and the query `implicitly[C]`.

This query would now be classified as ambiguous. This makes sense, after all
there are two possible solutions, `b(a1)` and `b(a2)`, neither of which is better
than the other and both of which are better than the third solution, `c`.
By contrast, Scala 2 would have rejected the search for `A` as
ambiguous, and subsequently have classified the query `b(implicitly[A])` as a normal fail,
which means that the alternative `c` would be chosen as solution!

Scala 2's somewhat puzzling behavior with respect to ambiguity has been exploited to implement
the analogue of a "negated" search in implicit resolution, where a query `Q1` fails if some
other query `Q2` succeeds and `Q1` succeeds if `Q2` fails. With the new cleaned up behavior
these techniques no longer work. But there is now a new special type [`scala.util.NotGiven`](https://scala-lang.org/api/3.x/scala/util/NotGiven.html)
which implements negation directly. For any query type `Q`, `NotGiven[Q]` succeeds if and only if
the implicit search for `Q` fails.

**5.** The treatment of divergence errors has also changed. A divergent implicit is treated as a normal failure, after which alternatives are still tried. This also makes sense: Encountering a divergent implicit means that we assume that no finite solution can be found on the corresponding path, but another path can still be tried. By contrast,
most (but not all) divergence errors in Scala 2 would terminate the implicit search as a whole.

**6.** Scala 2 gives a lower level of priority to implicit conversions with call-by-name parameters relative to implicit conversions with call-by-value parameters. Scala 3 drops this distinction. So the following code snippet would be ambiguous in Scala 3:

```scala
  implicit def conv1(x: Int): A = new A(x)
  implicit def conv2(x: => Int): A = new A(x)
  def buzz(y: A) = ???
  buzz(1)   // error: ambiguous
```
**7.** The rule for picking a _most specific_ alternative among a set of overloaded or implicit alternatives is refined to take context parameters into account. All else being equal, an alternative that takes some context parameters is taken to be less specific than an alternative that takes none. If both alternatives take context parameters, we try to choose between them as if they were methods with regular parameters. The following paragraph in the [SLS §6.26.3](https://scala-lang.org/files/archive/spec/2.13/06-expressions.html#overloading-resolution) is affected by this change:

_Original version:_

> An alternative A is _more specific_ than an alternative B if the relative weight of A over B is greater than the relative weight of B over A.

_Modified version:_

An alternative A is _more specific_ than an alternative B if

 - the relative weight of A over B is greater than the relative weight of B over A, or
 - the relative weights are the same, and A takes no implicit parameters but B does, or
 - the relative weights are the same, both A and B take implicit parameters, and A is more specific than B if all implicit parameters in either alternative are replaced by regular parameters.

**8.** The previous disambiguation of implicits based on inheritance depth is refined to make it transitive. Transitivity is important to guarantee that search outcomes are compilation-order independent. Here's a scenario where the previous rules violated transitivity:
```scala
  class A extends B
  object A { given a ... }
  class B
  object B extends C { given b ... }
  class C { given c }
```
 Here `a` is more specific than `b` since the companion class `A` is a subclass of the companion class `B`. Also, `b` is more specific than `c`
   since `object B` extends class `C`. But `a` is not more specific than `c`. This means if `a, b, c` are all applicable implicits, it makes
   a difference in what order they are compared. If we compare `b` and `c`
   first, we keep `b` and drop `c`. Then, comparing `a` with `b` we keep `a`. But if we compare `a` with `c` first, we fail with an ambiguity error.

The new rules are as follows: An implicit `a` defined in `A` is more specific than an implicit `b` defined in `B` if

 - `A` extends `B`, or
 - `A` is an object and the companion class of `A` extends `B`, or
 - `A` and `B` are objects,
    `B` does not inherit any implicit members from base classes (*),
    and the companion class of `A` extends the companion class of `B`.

Condition (*) is new. It is necessary to ensure that the defined relation is transitive.

[//]: # todo: expand with precise rules


**9.** Given disambiguation has changed. When comparing two givens that both match an expected type, we used to pick the most specific one, in alignment with
overloading resolution. From Scala 3.5 on, we pick the most general one instead. Compiling with Scala 3.5-migration will print a warning in all cases where the preference has changed. Example:
```scala
class A
class B extends A
class C extends A

given A = A()
given B = B()
given C = C()

summon[A]  // was ambiguous, will now return `given_A`
```

**10.** The following change is currently enabled in `-source future`:

Implicit resolution now avoids generating recursive givens that can lead to an infinite loop at runtime. Here is an example:

```scala
object Prices {
  opaque type Price = BigDecimal

  object Price{
    given Ordering[Price] = summon[Ordering[BigDecimal]] // was error, now avoided
  }
}
```

Previously, implicit resolution would resolve the `summon` to the given in `Price`, leading to an infinite loop (a warning was issued in that case). We now use the underlying given in `BigDecimal` instead. We achieve that by adding the following rule for implicit search:

 - When doing an implicit search while checking the implementation of a `given` definition `G` of the form
    ```
    given ... = ....
    ```
    discard all search results that lead back to `G` or to a given with the same owner as `G` that comes later in the source than `G`.

The new behavior is currently enabled in `source.future` and will be enabled at the earliest in Scala 3.6. For earlier source versions, the behavior is as
follows:

 - Scala 3.3: no change
 - Scala 3.4: A warning is issued where the behavior will change in 3.future.
 - Scala 3.5: An error is issued where the behavior will change in 3.future.

Old-style implicit definitions are unaffected by this change.

