---
layout: doc-page
title: "Type Lambdas"
nightlyOf: https://docs.scala-lang.org/scala3/reference/new-types/type-lambdas.html
---

A _type lambda_ lets one express a higher-kinded type directly, without
a type definition.

```scala sc:nocompile
[X, Y] =>> Map[Y, X]
```

For instance, the type above defines a binary type constructor, which maps arguments `X` and `Y` to `Map[Y, X]`.
Type parameters of type lambdas can have bounds, but they cannot carry `+` or `-` variance annotations.

[More details](./type-lambdas-spec.md)
