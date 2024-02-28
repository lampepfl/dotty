---
layout: doc-page
title: "Experimental Definitions"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/experimental-defs.html
---

The [`@experimental`](https://scala-lang.org/api/3.x/scala/annotation/experimental.html) annotation allows the definition of an API that is not guaranteed backward binary or source compatibility.
This annotation can be placed on term or type definitions.

## References to experimental definitions

Experimental definitions can only be referenced in an experimental scope. Experimental scopes are defined as follows:

1. The RHS of an experimental `def`, `val`, `var`, `given` or `type` is an experimental scope. Examples:

   <details>
   <summary>Example 1</summary>

   ```scala
   import scala.annotation.experimental

   @experimental
   def x = ()

   def d1 = x // error: value x is marked @experimental and therefore ...
   @experimental def d2 = x

   val v1 = x // error: value x is marked @experimental and therefore ...
   @experimental val v2 = x

   var vr1 = x // error: value x is marked @experimental and therefore ...
   @experimental var vr2 = x

   lazy val lv1 = x // error: value x is marked @experimental and therefore ...
   @experimental lazy val lv2 = x
   ```
   </details>

   <details>
   <summary>Example 2</summary>

   ```scala
   import scala.annotation.experimental

   @experimental
   val x = ()

   @experimental
   def f() = ()

   @experimental
   object X:
     def fx() = 1

   def test1: Unit =
     f() // error: def f is marked @experimental and therefore ...
     x // error: value x is marked @experimental and therefore ...
     X.fx() // error: object X is marked @experimental and therefore ...
     import X.fx
     fx() // error: object X is marked @experimental and therefore ...

   @experimental
   def test2: Unit =
     // references to f, x and X are ok because `test2` is experimental
     f()
     x
     X.fx()
     import X.fx
     fx()
   ```
   </details>

   <details>
   <summary>Example 3</summary>

   ```scala
   import scala.annotation.experimental

   @experimental type E

   type A = E // error type E is marked @experimental and therefore ...
   @experimental type B = E
   ```
   </details>

   <details>
   <summary>Example 4</summary>

   ```scala
   import scala.annotation.experimental

   @experimental class A
   @experimental type X
   @experimental type Y = Int
   @experimental opaque type Z = Int

   def test: Unit =
     new A // error: class A is marked @experimental and therefore ...
     val i0: A = ??? // error: class A is marked @experimental and therefore ...
     val i1: X = ??? // error: type X is marked @experimental and therefore ...
     val i2: Y = ??? // error: type Y is marked @experimental and therefore ...
     val i2: Z = ??? // error: type Y is marked @experimental and therefore ...
     ()
   ```
   </details>

   <details>
   <summary>Example 5</summary>

   ```scala
   @experimental
   trait ExpSAM {
     def foo(x: Int): Int
   }
   def bar(f: ExpSAM): Unit = {} // error: error form rule 2

   def test: Unit =
     bar(x => x) // error: reference to experimental SAM
     ()
   ```
   </details>

2. The signatures of an experimental `def`, `val`, `var`, `given` and `type`, or constructors of `class` and `trait` are experimental scopes. Examples:

   <details>
   <summary>Example 1</summary>

   ```scala
   import scala.annotation.experimental

   @experimental def x = 2
   @experimental class A
   @experimental type X
   @experimental type Y = Int
   @experimental opaque type Z = Int

   def test1(
     p1: A, // error: class A is marked @experimental and therefore ...
     p2: List[A], // error: class A is marked @experimental and therefore ...
     p3: X, // error: type X is marked @experimental and therefore ...
     p4: Y, // error: type Y is marked @experimental and therefore ...
     p5: Z, // error: type Z is marked @experimental and therefore ...
     p6: Any = x // error: def x is marked @experimental and therefore ...
   ): A = ??? // error: class A is marked @experimental and therefore ...

   @experimental def test2(
     p1: A,
     p2: List[A],
     p3: X,
     p4: Y,
     p5: Z,
     p6: Any = x
   ): A = ???

   class Test1(
     p1: A, // error
     p2: List[A], // error
     p3: X, // error
     p4: Y, // error
     p5: Z, // error
     p6: Any = x // error
   ) {}

   @experimental class Test2(
     p1: A,
     p2: List[A],
     p3: X,
     p4: Y,
     p5: Z,
     p6: Any = x
   ) {}

   trait Test1(
     p1: A, // error
     p2: List[A], // error
     p3: X, // error
     p4: Y, // error
     p5: Z, // error
     p6: Any = x // error
   ) {}

   @experimental trait Test2(
     p1: A,
     p2: List[A],
     p3: X,
     p4: Y,
     p5: Z,
     p6: Any = x
   ) {}
   ```
   </details>

3. The `extends` clause of an experimental `class`, `trait` or `object` is an experimental scope. Examples:

   <details>
   <summary>Example 1</summary>

   ```scala
   import scala.annotation.experimental

   @experimental def x = 2

   @experimental class A1(x: Any)
   class A2(x: Any)


   @experimental class B1 extends A1(1)
   class B2 extends A1(1) // error: class A1 is marked @experimental and therefore marked @experimental and therefore ...

   @experimental class C1 extends A2(x)
   class C2 extends A2(x) // error def x is marked @experimental and therefore
   ```
   </details>

4. The body of an experimental `class`, `trait` or `object` is an experimental scope. Examples:

   <details>
   <summary>Example 1</summary>

   ```scala
   import scala.annotation.experimental

   @experimental def x = 2

   @experimental class A {
     def f = x // ok because A is experimental
   }

   @experimental class B {
     def f = x // ok because A is experimental
   }

   @experimental object C {
     def f = x // ok because A is experimental
   }

   @experimental class D {
     def f = {
       object B {
         x // ok because A is experimental
       }
     }
   }
   ```

   </details>

5. Annotations of an experimental definition are in experimental scopes. Examples:

   <details>
   <summary>Example 1</summary>

   ```scala
   import scala.annotation.experimental

   @experimental class myExperimentalAnnot extends scala.annotation.Annotation

   @myExperimentalAnnot // error
   def test: Unit = ()

   @experimental
   @myExperimentalAnnot
   def test: Unit = ()
   ```

   </details>

6. Any code compiled using a [_Nightly_](https://search.maven.org/artifact/org.scala-lang/scala3-compiler_3) or _Snapshot_ version of the compiler is considered to be in an experimental scope.
Can use the `-Yno-experimental` compiler flag to disable it and run as a proper release.

7. An experimental language feature is imported in at the package level. All top-level definitions will be marked as `@experimental`.

In any other situation, a reference to an experimental definition will cause a compilation error.

## Experimental overriding

For an overriding member `M` and overridden member `O`, if `O` is non-experimental then `M` must be non-experimental.

This makes sure that we cannot have accidental binary incompatibilities such as the following change.
```diff
class A:
  def f: Any = 1
class B extends A:
-  @experimental def f: Int = 2
```

## Test frameworks

Tests can be defined as experimental. Tests frameworks can execute tests using reflection even if they are in an experimental class, object or method. Examples:

<details>
<summary>Example 1</summary>

Test that touch experimental APIs can be written as follows

```scala
import scala.annotation.experimental

@experimental def x = 2

class MyTests {
  /*@Test*/ def test1 = x // error
  @experimental /*@Test*/ def test2 = x
}

@experimental
class MyExperimentalTests {
  /*@Test*/ def test1 = x
  /*@Test*/ def test2 = x
}
```

</details>

## `-experimental` compiler flag

This flag enables the use of any experimental language feature in the project.
It does this by adding an `@experimental` annotation to all top-level definitions.
Hence, dependent projects also have to be experimental.
