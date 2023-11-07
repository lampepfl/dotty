---
layout: doc-page
title: "Quoted Patterns with Polymorphic Functions"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/quoted-patterns-with-polymorphic-functions.html
---

This feature extends the capability of quoted patterns with regard to polymorphic functions. It is not yet part of the Scala language standard. To use this feature, turn on the language feature [`experimental.quotedPatternsWithPolymorphicFunctions`](https://scala-lang.org/api/3.x/scala/runtime/stdLibPatches/language$$experimental$$quotedPatternsWithPolymorphicFunctions$.html). This can be done with a language import
```scala
import scala.language.experimental.quotedPatternsWithPolymorphicFunctions
```
or by setting the command line option `-language:experimental.quotedPatternsWithPolymorphicFunctions`.

## Background
Quoted patterns allows us to use quoted code as a pattern. Using quoted patterns, we can check if an expression is equivalent to another, or decompose it. Especially, higher-order patterns are useful when extracting code fraguments inside function bodies.

```scala
def decomposeFunc(x: Expr[Any])(using Quotes): Expr[Int] =
  x match
    case '{ (a: Int, b: Int) => $y(a, b) : Int } =>
      '{ $y(0, 0) }
    case _ => Expr(0)
```

In the example above, the first case matches the case where `x` is a function and `y` is bound to the body of the function. The higher-order pattern `$y(a, b)` states that it matches any code with free occurence of variables `a` and `b`. If it is `$y(a)` instead, an expression like `(a: Int, b: Int) => a + b` will not match because `a + b` has an occurence of `b`, which is not included in the higher-order pattern.

## Motivation
This experimental feature extends this higher-order pattern syntax to allow type variables.

```scala
def decomposePoly(x: Expr[Any])(using Quotes): Expr[Int] =
  x match
    case '{ [A] => (x: List[A]) => $y[A](x) : Int } =>
      '{ $y[Int](List(1, 2, 3)) }
    case _ => Expr(0)
```

Now we can use a higher-order pattern `$y[A](x)` with type variables. `y` is bound to the body of code with occurences of `A` and `x`, and has the type `[A] => (x: List[A]) => Int`.

## Type Dependency
If a higher-order pattern carries a value parameter with a type that has type parameters defined in the quoted pattern, those type parameters should also be captured in the higher-order pattern. For example, the following pattern will not be typed.

```
case '{ [A] => (x: List[A]) => $y(x) : Int } =>
```

In this case, `x` has the type `List[A]`, which includes a type variable `A` that is defined in the pattern. However, the higher-order pattern `$y(x)` does not have any type parameters. This should be ill-typed. One can always avoid this kind of type errors by adding type parameters, like `$y[A](x)`

## Implementation Restriction
Current implementation only allows type parameters that do not have bounds, because sound typing rules for such pattern is not clear yet.

```scala
case '{ [A] => (x: List[A]) => $y(x) : Int } => // Allowed
case '{ [A <: Int] => (x: List[A]) => $y(x) : Int } => // Disallowed
```
