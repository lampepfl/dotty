
---
layout: doc-page
title: "Type Classes"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/typeclasses.html
---

# Some Proposed Changes for Better Support of Type Classes

Martin Odersky, 8.1.2024

A type class in Scala is a pattern where we define

 - a trait with one type parameter (the _type class_)
 - given instances at specific instantiations of that trait,
 - using clauses or context bounds abstracting over that trait.

Type classes as a pattern work overall OK, but if we compare them to native implementations in Haskell, or protocols in Swift, or traits in Rust, then there are some idiosyncrasies and rough corners which in the end make them
a bit cumbersome and limiting for standard generic programming patterns. Much has improved since Scala 2's implicits, but there is still some gap to bridge to get to parity with these languages.

This note shows that with some fairly small and reasonable tweaks to Scala's syntax and typing rules we can obtain a much better scheme for working with type classes, or do generic programming in general.

The bulk of the suggested improvements has been implemented and is available
under source version `future` if the additional experimental language import `modularity` is present. For instance, using the following command:

```
  scala compile -source:future -language:experimental.modularity
```

## Generalizing Context Bounds

 The only place in Scala's syntax where the type class pattern is relevant is
 in context bounds. A context bound such as

```scala
   def min[A: Ordering](x: List[A]): A
```
requires that `Ordering` is a trait or class with a single type parameter (which makes it a type class) and expands to a `using` clause that instantiates that parameter. Here is the expansion of `min`:
```scala
   def min[A](x: List[A])(using Ordering[A]): A
```

**Proposal** Allow type classes to define an abstract type member named `Self` instead of a type parameter.

**Example**

```scala
  trait Ord:
    type Self

  trait SemiGroup:
    type Self
    extension (x: Self) def combine(y: Self): Self

  trait Monoid extends SemiGroup:
    def unit: Self
  object Monoid
    def unit(using m: Monoid): m.Self = m.unit

  trait Functor:
    type Self[A]
    extension [A](x: Self[A]) def map[B](f: A => B): Self[B]

  trait Monad extends Functor:
    def pure[A](x: A): Self[A]
    extension [A](x: Self[A])
      def flatMap[B](f: A => Self[B]): Self[B]
      def map[B](f: A => B) = x.flatMap(f `andThen` pure)

  def reduce[A: Monoid](xs: List[A]): A =
    xs.foldLeft(Monoid.unit)(_ `combine` _)

  trait ParserCombinator:
    type Self
    type Input
    type Result
    extension (self: Self)
      def parse(input: Input): Option[Result] = ...

  def combine[A: ParserCombinator, B: ParserCombinator { type Input = A.Input }] = ...
```

**Advantages**

 - Avoids repetitive type parameters, concentrates on what's essential, namely the type class hierarchy.
 - Gives a clear indication of traits intended as type classes. A trait is a type class
   if it has type `Self` as a member
 - Allows to create aggregate type classes that combine givens via intersection types.
 - Allows to use refinements in context bounds (the `combine` example above would be very awkward to express using the old way of context bounds expanding to type constructors).

`Self`-based context bounds are a better fit for a dependently typed language like Scala than parameter-based ones. The main reason is that we are dealing with proper types, not type constructors. Proper types can be parameterized, intersected, or refined. This makes `Self`-based designs inherently more compositional than parameterized ones.



**Details**

When a trait has both a type parameter and an abstract `Self` type, we
   resolve a context bound to the `Self` type. This allows type classes
   that carry type parameters, as in

```scala
trait Sequential[E]:
  type Self
```

Here,
```scala
[S: Sequential[Int]]
```
should resolve to:
```scala
[S](using Sequential[Int] { type Self = S })
```
and not to:
```scala
[S](using Sequential[S]
```

**Discussion**

 Why not use `This` for the self type? The name `This` suggests that it is the type of `this`. But this is not true for type class traits. `Self` is the name of the type implementing a distinguished _member type_ of the trait in a `given` definition. `Self` is an established term in both Rust and Swift with the meaning used here.

 One possible objection to the `Self` based design is that it does not cover "multi-parameter" type classes. But neither do context bounds! "Multi-parameter" type classes in Scala are simply givens that can be synthesized with the standard mechanisms. Type classes in the strict sense abstract only over a single type, namely the implementation type of a trait.


## Auxiliary Type Alias `is`

We introduce a standard type alias `is` in the Scala package or in `Predef`, defined like this:

```scala
  infix type is[A <: AnyKind, B <: {type Self <: AnyKind}] = B { type Self = A }
```

This makes writing instance definitions quite pleasant. Examples:

```scala
  given Int is Ord ...
  given Int is Monoid ...

  type Reader = [X] =>> Env => X
  given Reader is Monad ...
```

(more examples will follow below)



## Naming Context Bounds

Context bounds are a convenient and legible abbreviation. A problem so far is that they are always anonymous, one cannot name the using parameter to which a context bound expands. For instance, without the trick of defining a universal "trampoline" `unit` in the `Monoid` companion object, we would have to write `reduce` like this:
```scala
  def reduce[A](xs: List[A])(using m: A is Monoid) =
    xs.foldLeft(m.unit)(_ `combine` _)
```

**Proposal:** Allow to name a context bound, like this:
```scala
  def reduce[M: Monoid as m](xs: List[M] =
    xs.foldLeft(m.unit)(_ `combine` _)
```

We use `as x` after the type to bind the instance to `x`. This is analogous to import renaming, which also introduces a new name for something that comes before.

## New Syntax for Aggregate Context Bounds

Aggregate context bounds like `A: X: Y` are not obvious to read, and it becomes worse when we add names, e.g. `A : X as x : Y as y`.

**Proposal:** Allow to combine several context bounds inside `{...}`, analogous
to import clauses.

```scala
  def showMax[X : {Ordered as ordering, Show as show}](x: X, y: X): String =
    show.asString(ordering.max(x, y))
```

The old syntax with multiple `:` should be phased out over time.

## Better Default Names for Context Bounds

So far, a context bound for a type `M` gets a synthesized fresh name. It would be much more useful if it got the name of the constrained type instead, translated to be a term name. This means our `reduce` method over monoids would not even need an `as` binding. We could simply formulate it as follows:
```
 def reduce[M: Monoid](xs: List[M] =
    xs.foldLeft(M.unit)(_ `combine` _)
```

**Proposed Rules**

 1. The generated evidence parameter for a context bound `M: C as m` has name `m`
 2. The generated evidence for a context bound `M: C` without an `as` binding has name `M` (seen as a term name). So, `M: C` is equivalent to `M: C as M`.
 3. If there are more than one context bounds for a type parameter, the generated evidence parameter for every context bound except the first one has a fresh synthesized name, unless the context bound carries an `as` clause, in which case rule (1) applies.

## Expansion of Context Bounds

Context bounds are currently translated to implicit parameters in the last parameter list of a method or class. This is a problem if a context bound is mentioned in one of the preceding parameter types. Example:
```scala
  def f[C: ParserCombinator](x: C.Input) = ...
```
With the current translation, this would give
```scala
  def f[C](x: C.input)(using C: C is ParserCombinator)
```
But this is ill-typed, since the `C` in `C.input` refers to the `C` introduced in the using clause, which comes later.

This problem would be fixed by changing the translation of context bounds so that they expand to using clauses immediately after the type parameter. But such a change is
infeasible, for two reasons:

 1. It would be a binary-incompatible change.
 2. Putting using clauses earlier can impair type inference. A type in
    a using clause can be constrained by term arguments coming before that
    clause. Moving the using clause first would miss those constraints, which
    could cause ambiguities in implicit search.

But there is an alternative which is feasible:

**Proposal:** Map the context bounds of a method or class as follows:

 1. If one of the bounds is referred to by its term name in a subsequent parameter clause, the context bounds are mapped to a using clause immediately preceding the first such parameter clause.
 2. Otherwise, if the last parameter clause is a using (or implicit) clause, merge all parameters arising from context bounds in front of that clause, creating a single using clause.
 3. Otherwise, let the parameters arising from context bounds form a new using clause at the end.

Rules (2) and (3) are the status quo, and match Scala 2's rules. Rule (1) is new but since context bounds so far could not be referred to, it does not apply to legacy code. Therefore, binary compatibility is maintained.

**Discussion** More refined rules could be envisaged where context bounds are spread over different using clauses so that each comes as late as possible. But it would make matters more complicated and the gain in expressiveness is not clear to me.

Named (either explicitly, or by default) context bounds in givens that produce classes are mapped to tracked val's of these classes (see #18958). This allows
references to these parameters to be precise, so that information about dependent type members is preserved.


## Context Bounds for Type Members

It's not very orthogonal to allow subtype bounds for both type parameters and abstract type members, but context bounds only for type parameters. By moving more of the type class logic to type members this lack becomes more of a problem.

**Proposal**: Allow context bounds for type members. Example:

```scala
  class C:
    type Element: Ordered
```

The question is how these bounds are expanded. Context bounds on type parameters
are expanded into using clauses. But for type members this does not work, since we cannot refer to a member type of a class in a parameter type of that class. What we are after is an equivalent of using parameter clauses but represented as class members. This is basically a list of deferred givens which can be filled automatically on object creation.

**Proposal:** Introduce a new kind of given definition of the form:
```scala
given T = deferred
```
`deferred` is a soft keyword which has special meaning only in this context.
A given with `deferred` right hand side can appear only as a member definition of some trait. Any class implementing that trait will provide an implementation of this given. If a definition is not provided explicitly, it will be synthesized by searching for a given of type `T` in the scope of the inheriting class `C`. Specifically, the scope in which this given will be searched is the environment of `C` augmented by the parameters of `C` but not containing the members of `C` (since that would lead to recursive resolutions).

A context bound
```scala
type T: C
```
in a trait will then expand to
```scala
type T
given T is C = deferred
```


## New Given Syntax

Our goal is to revise the `given` syntax so that conditional givens don't have to be written anymore like this:

```scala
given [A](using Ord[A]): Ord[List[A]] with
  def compare(x: List[A], y: List[A]) = ...
```
or, using an explicit name, like this:
```scala
given listOrd[A](using Ord[A]): Ord[List[A]] with
  def compare(x: List[A], y: List[A]) = ...
```
The named version below makes sort of sense, but the unnamed version above is annoyingly irregular compared to other Scala language elements. In particular:

 - There is the `:` that separates parameter clauses and the implemented type class.
   This feels out of place, in particular in the unnamed version.
 - There is the `with` at the end, which is the only place in Scala where `with` starts a block with definitions. Everywhere else we use `:` or `=`.

The awkwardness of the given syntax was forced upon us since we insisted that givens could be named or anonymous, that we would not use underscore for an anonymous given, and that the name, if present, had to come first. Things become much simpler if we introduce the optional name instead with an `as name` clause at the end. We get uniformity with context bounds,
and we can use a more intuitive syntax for givens like this:

```scala
given Int is Ord:
  def compare(x: A, y: A) = ...

given [A: Ord] => List[A] is Ord:
  def compare(x: A, y: A) =
    ...

given Int is Monoid as intMonoid:
  extension (x: Int) def combine(y: Int) = x + y
  def unit = 0
```

The underlying principles are:

 - A `given` clause consists of the following elements:

    - An optional _precondition_, which introduces type parameters and/or using clauses and which ends in `=>`,
    - the implemented _type_,
    - an optional name binding using `as`,
    - an implementation which consists of either an `=` and an expression,
      or a template body.

 - We get the pleasing `<instance-type> is <type-class>` syntax simply by using the predefined infix type `is`.
 - Since there is no more middle `:` separating name and parameters from the implemented type, we can use a `:` to start the class body without looking unnatural. That eliminates the special case where `with` was used before.

This will be a fairly significant change to the given syntax. I believe there's still a possibility to do this,
if we are convinced that this is a clear improvement. Not so much code has migrated to new style givens yet, and code that was written can probably be changed fairly easily. But we the longer we wait, the harder it would get.

Migration to the new syntax should be straightforward. For a transition period we can support both the old and the new syntax. And we can offer rewrite rules that turn old into new.

## Abolish Abstract Givens

So far we have special syntax for abstract givens:
```scala
given x: T
```
This should be no longer necessary with the `deferred` scheme, so I propose to deprecate that syntax (over time). Abstract givens are a highly specialized mechanism with a (so far) non-obvious syntax. My estimate is that maybe a dozen people world-wide have used them in anger so far.

**Proposal** In the future, let the `= deferred` mechanism be the only way to define an abstract given.

This is less of a disruption than it might appear at first:

 - `given T` was illegal before since abstract givens could not be anonymous.
   It now means a concrete given of class `T` with no member definitions. This
   is the natural interpretation for simple tagging given clauses such as
   `given String is Value`.
 - `given x: T` is legacy syntax for a deferred given.
 - `given T as x = deferred` is the analogous new syntax, which is more powerful since
    it allows for automatic instantiation.
 - `given T = deferred` is the anonymous version in the new syntax, which was not expressible before.

## Syntax Changes

The changed syntax is here. Overall the syntax for givens becomes a lot simpler than what it was before.

```
TmplDef           ::=  'given' GivenDef
GivenDef          ::=  [GivenConditional '=>'] GivenSig
GivenConditional  ::=  [DefTypeParamClause | UsingParamClause] {UsingParamClause}
GivenSig          ::=  GivenType ['as' id] ([‘=’ Expr] | TemplateBody)
                   |   ConstrApps ['as' id] TemplateBody
GivenType         ::=  AnnotType {id [nl] AnnotType}

TypeParamBounds   ::=  TypeBounds [‘:’ ContextBounds]
ContextBounds     ::=  ContextBound | '{' ContextBound {',' ContextBound} '}'
ContextBound      ::=  Type ['as' id]
```



## Examples


### Example 1

Here are some standard type classes, which were mostly already introduced at the start of this note, now with associated instance givens and some test code:

```scala
  // Type classes

  trait Ord:
    type Self
    extension (x: Self)
      def compareTo(y: Self): Int
      def < (y: Self): Boolean = compareTo(y) < 0
      def > (y: Self): Boolean = compareTo(y) > 0
      def <= (y: Self): Boolean = compareTo(y) <= 0
      def >= (y: Self): Boolean = compareTo(y) >= 0
      def max(y: Self): Self = if x < y then y else x

  trait Show:
    type Self
    extension (x: Self) def show: String

  trait SemiGroup:
    type Self
    extension (x: Self) def combine(y: Self): Self

  trait Monoid extends SemiGroup:
    def unit: Self

  trait Functor:
    type Self[A] // Here, Self is a type constructor with parameter A
    extension [A](x: Self[A]) def map[B](f: A => B): Self[B]

  trait Monad extends Functor:
    def pure[A](x: A): Self[A]
    extension [A](x: Self[A])
      def flatMap[B](f: A => Self[B]): Self[B]
      def map[B](f: A => B) = x.flatMap(f `andThen` pure)

  // Instances

  given Int is Ord:
    extension (x: Int)
      def compareTo(y: Int) =
        if x < y then -1
        else if x > y then +1
        else 0

  given [T: Ord] => List[T] is Ord:
    extension (xs: List[T]) def compareTo(ys: List[T]): Int =
      (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)

  given List is Monad:
    extension [A](xs: List[A])
      def flatMap[B](f: A => List[B]): List[B] =
        xs.flatMap(f)
    def pure[A](x: A): List[A] =
      List(x)

  type Reader[Ctx] = [X] =>> Ctx => X

  given [Ctx] => Reader[Ctx] is Monad:
    extension [A](r: Ctx => A)
      def flatMap[B](f: A => Ctx => B): Ctx => B =
        ctx => f(r(ctx))(ctx)
    def pure[A](x: A): Ctx => A =
      ctx => x

  // Usages

  extension (xs: Seq[String])
    def longestStrings: Seq[String] =
      val maxLength = xs.map(_.length).max
      xs.filter(_.length == maxLength)

  extension [M[_]: Monad, A](xss: M[M[A]])
    def flatten: M[A] =
      xss.flatMap(identity)

  def maximum[T: Ord](xs: List[T]): T =
    xs.reduce(_ `max` _)

  given [T: Ord] => T is Ord as descending:
    extension (x: T) def compareTo(y: T) = T.compareTo(y)(x)

  def minimum[T: Ord](xs: List[T]) =
    maximum(xs)(using descending)
```


### Example 2

The following contributed code by @LPTK (issue #10929) did _not_ work at first since
references were not tracked correctly. The version below adds explicit tracked parameters which makes the code compile.
```scala
infix abstract class TupleOf[T, +A]:
  type Mapped[+A] <: Tuple
  def map[B](x: T)(f: A => B): Mapped[B]

object TupleOf:

  given TupleOf[EmptyTuple, Nothing] with
    type Mapped[+A] = EmptyTuple
    def map[B](x: EmptyTuple)(f: Nothing => B): Mapped[B] = x

  given [A, Rest <: Tuple](using tracked val tup: Rest TupleOf A): TupleOf[A *: Rest, A] with
    type Mapped[+A] = A *: tup.Mapped[A]
    def map[B](x: A *: Rest)(f: A => B): Mapped[B] =
      f(x.head) *: tup.map(x.tail)(f)
```

Note the quite convoluted syntax, which makes the code hard to understand. Here is the same example in the new type class syntax, which also compiles correctly:
```scala
//> using options -language:experimental.modularity -source future

trait TupleOf[+A]:
  type Self
  type Mapped[+A] <: Tuple
  def map[B](x: Self)(f: A => B): Mapped[B]

object TupleOf:

  given EmptyTuple is TupleOf[Nothing]:
    type Mapped[+A] = EmptyTuple
    def map[B](x: EmptyTuple)(f: Nothing => B): Mapped[B] = x

  given [A, Rest <: Tuple : TupleOf[A]] => A *: Rest is TupleOf[A]:
    type Mapped[+A] = A *: Rest.Mapped[A]
    def map[B](x: A *: Rest)(f: A => B): Mapped[B] =
      f(x.head) *: Rest.map(x.tail)(f)
```
Note in particular the following points:

 - In the original code, it was not clear that `TupleOf` is a type class,
 since it contained two type parameters, one of which played the role
 of the instance type `Self`. The new version is much clearer: `TupleOf` is
 a type class over `Self` with one additional parameter, the common type of all tuple elements.
 - The two given definitions are obfuscated in the old code. Their version
 in the new code makes it clear what kind of instances they define:

   - `EmptyTuple` is a tuple of `Nothing`.
   - if `Rest` is a tuple of `A`, then `A *: Rest` is also a tuple of `A`.

 - There's no need to introduce names for parameter instances in using clauses; the default naming scheme for context bound evidences works fine, and is more concise.
 - There's no need to manually declare implicit parameters as `tracked`,
   context bounds provide that automatically.
 - Everything in the new code feels like idiomatic Scala 3, whereas the original code exhibits the awkward corner case that requires a `with` in
 front of given definitions.

### Example 3

Dimi Racordon tried to [define parser combinators](https://users.scala-lang.org/t/create-an-instance-of-a-type-class-with-methods-depending-on-type-members/9613) in Scala that use dependent type members for inputs and results. It was intended as a basic example of type class constraints, but it did not work in current Scala.

Here is the problem solved with the new syntax. Note how much clearer that syntax is compared to Dimi's original version, which did not work out in the end.

```scala
/** A parser combinator */
trait Combinator:
  type Self

  type Input
  type Result

  extension (self: Self)
    /** Parses and returns an element from input `in` */
    def parse(in: Input): Option[Result]
end Combinator

case class Apply[I, R](action: I => Option[R])
case class Combine[A, B](a: A, b: B)

given [I, R] => Apply[I, R] is Combinator:
  type Input = I
  type Result = R
  extension (self: Apply[I, R])
    def parse(in: I): Option[R] = self.action(in)

given [A: Combinator, B: Combinator { type Input = A.Input }]
    => Combine[A, B] is Combinator:
  type Input = A.Input
  type Result = (A.Result, B.Result)
  extension (self: Combine[A, B])
    def parse(in: Input): Option[Result] =
      for
        x <- self.a.parse(in)
        y <- self.b.parse(in)
      yield (x, y)
```
The example is now as expressed as straightforwardly as it should be:

 - `Combinator` is a type class with two associated types, `Input` and `Result`, and a `parse` method.
 - `Apply` and `Combine` are two data constructors representing parser combinators. They are declared to be `Combinators`  in the two subsequent `given` declarations.
 - `Apply`'s parse method applies the `action` function to the input.
 - `Combine[A, B]` is a parser combinator provided `A` and `B` are parser combinators
   that process the same type of `Input`, which is also the input type of
   `Combine[A, B]`. Its `Result` type is a pair of the `Result` types of `A` and `B`.
   Results are produced by a simple for-expression.

Compared to the original example, which required serious contortions, this is now all completely straightforward.

_Note 1:_ One could also explore improvements, for instance making this purely functional. But that's not the point of the demonstration here, where I wanted
to take the original example and show how it can be made to work with the new constructs, and be expressed more clearly as well.

_Note 2:_ One could improve the notation even further by adding equality constraints in the style of Swift, which in turn resemble the _sharing constraints_ of SML. A hypothetical syntax applied to the second given would be:
```scala
given [A: Combinator, B: Combinator with A.Input == B.Input]
    => Combine[A, B] is Combinator:
```
This variant is aesthetically pleasing since it makes the equality constraint symmetric. The original version had to use an asymmetric refinement on the second type parameter bound instead. For now, such constraints are neither implemented nor proposed. This is left as a possibility for future work. Note also the analogy with
the work of @mbovel and @Sporarum on refinement types, where similar `with` clauses can appear for term parameters. If that work goes ahead, we could possibly revisit the issue of `with` clauses also for type parameters.

### Example 4

Dimi Racordon tried to [port some core elements](https://github.com/kyouko-taiga/scala-hylolib) of the type class based [Hylo standard library to Scala](https://github.com/hylo-lang/hylo/tree/main/StandardLibrary/Sources). It worked to some degree, but there were some things that could not be expressed, and more things that could be expressed only awkwardly.

With the improvements proposed here, the library can now be expressed quite clearly and straightforwardly. See tests/pos/hylolib in this PR for details.

## Suggested Improvements unrelated to Type Classes

The following improvements elsewhere would make sense alongside the suggested changes to type classes. But they are currently not part of this proposal or implementation.

### Fixing Singleton

We know the current treatment of `Singleton` as a type bound is broken since
`x.type | y.type <: Singleton` holds by the subtyping rules for union types, even though `x.type | y.type` is clearly not a singleton.

A better approach is to treat `Singleton` as a type class that is interpreted specially by the compiler.

We can do this in a backwards-compatible way by defining `Singleton` like this:

```scala
trait Singleton:
  type Self
```

Then, instead of using an unsound upper bound we can use a context bound:

```scala
def f[X: Singleton](x: X) = ...
```

The context bound would be treated specially by the compiler so that no using clause is generated at runtime.

_Aside_: This can also lead to a solution how to express precise type variables. We can introduce another special type class `Precise` and use it like this:

```scala
def f[X: Precise](x: X) = ...
```
This would disable automatic widening of singleton types in inferred instances of type variable `X`.

### Using `as` also in Patterns

Since we have now more precedents of `as` as a postfix binder, I want to come back to the proposal to use it in patterns as well, in favor of `@`, which should be deprecated.

Examples:

```scala
  xs match
    case (Person(name, age) as p) :: rest => ...

  tp match
    case Param(tl, _) :: _ as tparams => ...

  val x :: xs1 as xs = ys.checkedCast
```

These would replace the previous syntax using `@`:

```scala
  xs match
    case p @ Person(name, age) :: rest => ...

  tp match
    case tparams @ (Param(tl, _) :: _) => ...

  val xs @ (x :: xs1) = ys.checkedCast
```
**Advantages:** No unpronounceable and non-standard symbol like `@`. More regularity.

Generally, we want to use `as name` to attach a name for some entity that could also have been used stand-alone.

**Proposed Syntax Change**

```
Pattern2          ::=  InfixPattern ['as' id]
```

## Summary

I have proposed some tweaks to Scala 3, which would greatly increase its usability for modular, type class based, generic programming. The proposed changes are:

 1. Allow context bounds over classes that define a `Self` member type.
 1. Allow context bounds to be named with `as`. Use the bound parameter name as a default name for the generated context bound evidence.
 1. Add a new `{...}` syntax for multiple context bounds.
 1. Make context bounds also available for type members, which expand into a new form of deferred given. Phase out the previous abstract givens in favor of the new form.
 1. Add a predefined type alias `is`.
 1. Introduce a new cleaner syntax of given clauses.

It's interesting that givens, which are a very general concept in Scala, were "almost there" when it comes to full support of concepts and generic programming. We only needed to add a few usability tweaks to context bounds,
alongside two syntactic changes that supersede the previous forms of `given .. with` clauses and abstract givens. Also interesting is that the superseded syntax constructs were the two areas where we collectively felt that the previous solutions were a bit awkward, but we could not think of better ones at the time. It's very nice that more satisfactory solutions are now emerging.

## Conclusion

Generic programming can be expressed in a number of languages. For instance, with
type classes in Haskell, or with traits in Rust, or with protocols in Swift, or with concepts in C++. Each of these is constructed from a fairly heavyweight set of new constructs, different from expressions and types. By contrast, equivalent solutions in Scala rely on regular types. Type classes are simply traits that define a `Self` type member.

The proposed scheme has similar expressiveness to Protocols in Swift or Traits in Rust. Both of these were largely influenced by Jeremy Siek's PdD thesis "[A language for generic programming](https://scholarworks.iu.edu/dspace/handle/2022/7067)", which was first proposed as a way to implement concepts in C++. C++ did not follow Siek's approach, but Swift and Rust did.

In Siek's thesis and in the formal treatments of Rust and Swift,
 type class concepts are explained by mapping them to a lower level language of explicit dictionaries with representations for terms and types. Crucially, that lower level is not expressible without loss of granularity in the source language itself, since type representations are mapped to term dictionaries. By contrast, the current proposal expands type class concepts into other well-typed Scala constructs, which ultimately map into well-typed DOT programs. Type classes are simply a convenient notation for something that can already be expressed in Scala. In that sense, we stay true to the philosophy of a _scalable language_, where a small core can support a large range of advanced use cases.

