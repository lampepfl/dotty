---
layout: doc-page
title: "Type Lambdas - More Details"
nightlyOf: https://docs.scala-lang.org/scala3/reference/new-types/type-lambdas-spec.html
---

## Syntax

```ebnf
Type            ::=  ... |  TypeParamClause ‘=>>’ Type
TypeParamClause ::=  ‘[’ TypeParam {‘,’ TypeParam} ‘]’
TypeParam       ::=  {Annotation} (id [HkTypeParamClause] | ‘_’) TypeBounds
TypeBounds      ::=  [‘>:’ Type] [‘<:’ Type]
```

## Type Checking

A type lambda such as `[X] =>> F[X]` defines a function from types to types. The parameter(s) may carry bounds.
If a parameter is bounded, as in `[X >: L <: U] =>> F[X]` it is checked that arguments to the parameters conform to the bounds `L` and `U`.
Only the upper bound `U` can be F-bounded, i.e. `X` can appear in it.

## Subtyping Rules

Assume two type lambdas
```scala sc:nocompile
type TL1  =  [X >: L1 <: U1] =>> R1
type TL2  =  [X >: L2 <: U2] =>> R2
```
Then `TL1 <: TL2`, if

 - the type interval `L2..U2` is contained in the type interval `L1..U1` (i.e.
`L1 <: L2` and `U2 <: U1`),
 - `R1 <: R2`

Here we have relied on [alpha renaming](https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B1-conversion) to match the two bound types `X`.

A partially applied type constructor such as `List` is assumed to be equivalent to
its eta expansion. I.e, `List = [X] =>> List[X]`. This allows type constructors to be compared with type lambdas.

## Relationship with Parameterized Type Definitions

A parameterized type definition
```scala sc:nocompile
type T[X] = R
```
is regarded as a shorthand for an unparameterized definition with a type lambda as right-hand side:
```scala sc:nocompile
type T = [X] =>> R
```
If the type definition carries `+` or `-` variance annotations,
it is checked that the variance annotations are satisfied by the type lambda.
For instance,
```scala
type F2[A, +B] = A => B
```
expands to
```scala
type F2 = [A, B] =>> A => B
```
and at the same time it is checked that the parameter `B` appears covariantly in `A => B`.

A parameterized abstract type
```scala sc:nocompile
type T[X] >: L <: U
```
is regarded as shorthand for an unparameterized abstract type with type lambdas as bounds.
```scala sc:nocompile
type T >: ([X] =>> L) <: ([X] =>> U)
```
However, if `L` is `Nothing` it is not parameterized, since `Nothing` is treated as a bottom type for all kinds. For instance,
```scala sc:nocompile
type T[X] <: X => X
```
is expanded to
```scala
type T >: Nothing <: ([X] =>> X => X)
```
instead of
```scala
type T >: ([X] =>> Nothing) <: ([X] =>> X => X)
```

The same expansions apply to type parameters. For instance,
```scala sc:nocompile
[F[X] <: Coll[X]]
```
is treated as a shorthand for
```scala sc:nocompile
[F >: Nothing <: [X] =>> Coll[X]]
```
Abstract types and opaque type aliases remember the variances they were created with. So the type
```scala sc:nocompile
type F2[-A, +B]
```
is known to be contravariant in `A` and covariant in `B` and can be instantiated only
with types that satisfy these constraints. Likewise
```scala sc:nocompile
opaque type O[X] = List[X]
```
`O` is known to be invariant (and not covariant, as its right-hand side would suggest). On the other hand, a transparent alias
```scala
type O2[X] = List[X]
```
would be treated as covariant, `X` is used covariantly on its right-hand side.

**Note:** The decision to treat `Nothing` as universal bottom type is provisional, and might be changed after further discussion.

**Note:** Scala 2 and 3 differ in that Scala 2 also treats `Any` as universal top-type. This is not done in Scala 3. See also the discussion on [kind polymorphism](../other-new-features/kind-polymorphism.md)

## Curried Type Parameters

The body of a type lambda can again be a type lambda. Example:
```scala
type TL = [X] =>> [Y] =>> (X, Y)
```
Currently, no special provision is made to infer type arguments to such curried type lambdas. This is left for future work.
