---
layout: doc-page
title: "Dependent Function Types - More Details"
nightlyOf: https://docs.scala-lang.org/scala3/reference/new-types/dependent-function-types-spec.html
---

Initial implementation in [PR #3464](https://github.com/scala/scala3/pull/3464).

## Syntax

```ebnf
FunArgTypes       ::=  InfixType
                    |  ‘(’ [ FunArgType {',' FunArgType } ] ‘)’
                    |  ‘(’ TypedFunParam {',' TypedFunParam } ‘)’
TypedFunParam     ::=  id ‘:’ Type
```

Dependent function types associate to the right, e.g.
`(s: S) => (t: T) => U` is the same as `(s: S) => ((t: T) => U)`.

## Implementation

Dependent function types are shorthands for class types that define `apply`
methods with a dependent result type. Dependent function types desugar to
refinement types of `scala.FunctionN`. A dependent function type
`(x1: K1, ..., xN: KN) => R` of arity `N` translates to:

```scala sc:nocompile
FunctionN[K1, ..., Kn, R']:
  def apply(x1: K1, ..., xN: KN): R
```

where the result type parameter `R'` is the least upper approximation of the
precise result type `R` without any reference to value parameters `x1, ..., xN`.

The syntax and semantics of anonymous dependent functions is identical to the
one of regular functions. Eta expansion is naturally generalized to produce
dependent function types for methods with dependent result types.

Dependent functions can be implicit, and generalize to arity `N > 22` in the
same way that other functions do, see
[the corresponding documentation](../dropped-features/limit22.md).

## Examples

The example below defines a trait `C` and the two dependent function types
`DF` and `IDF` and prints the results of the respective function applications:

[depfuntype.scala]: https://github.com/scala/scala3/blob/main/tests/pos/depfuntype.scala

```scala
trait C { type M; val m: M }

type DF = (x: C) => x.M

type IDF = (x: C) ?=> x.M

@main def test =
  val c = new C { type M = Int; val m = 3 }

  val depfun: DF = (x: C) => x.m
  val t = depfun(c)
  println(s"t=$t")   // prints "t=3"

  val idepfun: IDF = summon[C].m
  val u = idepfun(using c)
  println(s"u=$u")   // prints "u=3"

```

In the following example the depend type `f.Eff` refers to the effect type `CanThrow`:

[eff-dependent.scala]: https://github.com/scala/scala3/blob/main/tests/run/eff-dependent.scala

```scala
trait Effect

// Type X => Y
abstract class Fun[-X, +Y]:
  type Eff <: Effect
  def apply(x: X): Eff ?=> Y

class CanThrow extends Effect
class CanIO extends Effect

given ct: CanThrow = new CanThrow
given ci: CanIO = new CanIO

class I2S extends Fun[Int, String]:
  type Eff = CanThrow
  def apply(x: Int) = x.toString

class S2I extends Fun[String, Int]:
  type Eff = CanIO
  def apply(x: String) = x.length

// def map(f: A => B)(xs: List[A]): List[B]
def map[A, B](f: Fun[A, B])(xs: List[A]): f.Eff ?=> List[B] =
  xs.map(f.apply)

// def mapFn[A, B]: (A => B) -> List[A] -> List[B]
def mapFn[A, B]: (f: Fun[A, B]) => List[A] => f.Eff ?=> List[B] =
  f => xs => map(f)(xs)

// def compose(f: A => B)(g: B => C)(x: A): C
def compose[A, B, C](f: Fun[A, B])(g: Fun[B, C])(x: A):
  f.Eff ?=> g.Eff ?=> C =
  g(f(x))

// def composeFn: (A => B) -> (B => C) -> A -> C
def composeFn[A, B, C]:
  (f: Fun[A, B]) => (g: Fun[B, C]) => A => f.Eff ?=> g.Eff ?=> C =
  f => g => x => compose(f)(g)(x)

@main def test =
  val i2s = new I2S
  val s2i = new S2I

  assert(mapFn(i2s)(List(1, 2, 3)).mkString == "123")
  assert(composeFn(i2s)(s2i)(22) == 2)
```

## Type Checking

After desugaring no additional typing rules are required for dependent function types.
