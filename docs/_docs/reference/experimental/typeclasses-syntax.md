
# Pre-SIP: Improve Syntax for Context Bounds and Givens

Some months ago I experimented with some small extensions and tweaks to Scala that would make Scala's support for type classes much more pleasant. It would put it on a par of what is supported by Rust or Swift but at the same time be distinctly Scala like, with simple and concise syntax and clear semantic foundations.

That experiment consisted of three areas which are independent of each other:

 1. Allow context bounds also for types with `Self` type members.
 2. Better support for modularity: keep track of the types of certain class arguments.
 3. Syntactic improvements: Named context bounds, simpler and more regular syntax for givens.

I would like to propose each of these areas as separate SIPs. That makes them easier to review and discuss. Furthermore, each change set has value independently of what happens to the other proposals.

I'll start with (3. syntactic improvements) which is the largest chunk of changes and also the most time sensitive. It contains new syntax that supersedes some existing syntax that was introduced in 3.0, so it's better to make the change at a time when not that much code using the new syntax is written yet.
By contrast the other two areas are less urgent. The set of changes proposed here are valuable to have, even if the other two areas are not, or not yet, accepted.

The proposal is structured in three parts, covering named context bounds,
context bounds for type members, and changes to the given syntax.

## 1. Named Context Bounds

Context bounds are a convenient and legible abbreviation. A problem so far is that they are always anonymous, one cannot name the implicit parameter to which a context bound expands. For instance, consider the classical pair of type classes
```scala
  trait SemiGroup[A]:
    extension (x: A) def combine(y: A): A

  trait Monoid[A] extends SemiGroup[A]:
    def unit: A
```
and a `reduce` method defined like this:
```scala
def reduce[A : Monoid](xs: List[A]): A = ???
```
Since we don't have a name for the `Monoid` instance of `A`, we need to resort to `summon` in the body of `reduce`:
```scala
def reduce[A : Monoid](xs: List[A]): A =
  xs.foldLeft(summon[Monoid[A]].unit)(_ `combine` _)
```
That's generally considered too painful to write and read, hence people usually adopt one of two alternatives. Either, eschew context bounds and switch to using clauses:
```scala
def reduce[A](xs: List[A])(using m: Monoid[A]): A =
  xs.foldLeft(m)(_ `combine` _)
```
Or, plan ahead and define a "trampoline" method in `Monoid`'s companion object:
```scala
  trait Monoid[A] extends SemiGroup[A]:
    def unit: A
  object Monoid:
    def unit[A](using m: Monoid[A]): A = m.unit
  ...
  def reduce[A : Monoid](xs: List[A]): A =
    xs.foldLeft(Monoid.unit)(_ `combine` _)
```
This is all accidental complexity which can be avoided by the following proposal.

**Proposal:** Allow to name a context bound, like this:
```scala
  def reduce[A : Monoid as m](xs: List[A]): A =
    xs.foldLeft(m.unit)(_ `combine` _)
```

We use `as x` after the type to bind the instance to `x`. This is analogous to import renaming, which also introduces a new name for something that comes before.

**Benefits:** The new syntax is simple and clear. It avoids the awkward choice between concise context bounds that can't be named and verbose using clauses that can.

### New Syntax for Aggregate Context Bounds

Aggregate context bounds like `A : X : Y` are not obvious to read, and it becomes worse when we add names, e.g. `A : X as x : Y as y`.

**Proposal:** Allow to combine several context bounds inside `{...}`, analogous
to import clauses. Example:

```scala
  trait:
    def showMax[X : {Ordering, Show}](x: X, y: X): String
  class B extends A:
    def showMax[X : {Ordering as ordering, Show as show}](x: X, y: X): String =
      show.asString(ordering.max(x, y))
```

The old syntax with multiple `:` should be phased out over time. There's more about migration at the end of this Pre-SIP.

**Benefits:** The new syntax is much clearer than the old one, in particular for newcomers that don't know context bounds well.

### Better Default Names for Context Bounds

So far, an unnamed context bound for a type parameter gets a synthesized fresh name. It would be much more useful if it got the name of the constrained type parameter instead, translated to be a term name. This means our `reduce` method over monoids would not even need an `as` binding. We could simply formulate it as follows:
```
 def reduce[A : Monoid](xs: List[A]) =
    xs.foldLeft(A.unit)(_ `combine` _)
```



The use of a name like `A` above in two variants, both as a type name and as a term name is of course familiar to Scala programmers. We use the same convention for classes and companion objects. In retrospect, the idea of generalizing this to also cover type parameters is obvious. It is surprising that it was not brought up before.

**Proposed Rules**

 1. The generated evidence parameter for a context bound `A : C as a` has name `a`
 2. The generated evidence for a context bound `A : C` without an `as` binding has name `A` (seen as a term name). So, `A : C` is equivalent to `A : C as A`.
 3. If there are multiple context bounds for a type parameter, as in `A : {C_1, ..., C_n}`, the generated evidence parameter for every context bound `C_i` has a fresh synthesized name, unless the context bound carries an `as` clause, in which case rule (1) applies.

The default naming convention reduces the need for named context bounds. But named context bounds are still essential, for at least two reasons:

 - They are needed to give names to multiple context bounds.
 - They give an explanation what a single unnamed context bound expands to.


### Expansion of Context Bounds

Context bounds are currently translated to implicit parameters in the last parameter list of a method or class. This is a problem if a context bound is mentioned in one of the preceding parameter types. For example, consider a type class of parsers with associated type members `Input` and `Result` describing the input type on which the parsers operate and the type of results they produce:
```scala
trait Parser[P]:
  type Input
  type Result
```
Here is a method `run` that runs a parser on an input of the required type:

```scala
def run[P : Parser](in: P.Input): P.Result
```
Or, making clearer what happens by using an explicit name for the context bound:
```scala
def run[P : Parser as p](in: p.Input): p.Result
```
With the current translation this does not work since it would be expanded to:
```scala
  def run[P](x: p.Input)(using p: Parser[P]): p.Result
```
Note that the `p` in `p.Input` refers to the `p` introduced in the using clause, which comes later. So this is ill-formed.

This problem would be fixed by changing the translation of context bounds so that they expand to using clauses immediately after the type parameter. But such a change is infeasible, for two reasons:

 1. It would be a binary-incompatible change.
 2. Putting using clauses earlier can impair type inference. A type in
    a using clause can be constrained by term arguments coming before that
    clause. Moving the using clause first would miss those constraints, which could cause ambiguities in implicit search.

But there is an alternative which is feasible:

**Proposal:** Map the context bounds of a method or class as follows:

 1. If one of the bounds is referred to by its term name in a subsequent parameter clause, the context bounds are mapped to a using clause immediately preceding the first such parameter clause.
 2. Otherwise, if the last parameter clause is a using (or implicit) clause, merge all parameters arising from context bounds in front of that clause, creating a single using clause.
 3. Otherwise, let the parameters arising from context bounds form a new using clause at the end.

Rules (2) and (3) are the status quo, and match Scala 2's rules. Rule (1) is new but since context bounds so far could not be referred to, it does not apply to legacy code. Therefore, binary compatibility is maintained.

**Discussion** More refined rules could be envisaged where context bounds are spread over different using clauses so that each comes as late as possible. But it would make matters more complicated and the gain in expressiveness is not clear to me.


## 2. Context Bounds for Type Members

It's not very orthogonal to allow subtype bounds for both type parameters and abstract type members, but context bounds only for type parameters. What's more, we don't even have the fallback of an explicit using clause for type members. The only alternative is to also introduce a set of abstract givens that get implemented in each subclass. This is extremely heavyweight and opaque to newcomers.

**Proposal**: Allow context bounds for type members. Example:

```scala
  class Collection:
    type Element : Ord
```

The question is how these bounds are expanded. Context bounds on type parameters
are expanded into using clauses. But for type members this does not work, since we cannot refer to a member type of a class in a parameter type of that class. What we are after is an equivalent of using parameter clauses but represented as class members.

**Proposal:** ~~Introduce a new kind of given definition of the form:~~
Introduce a new way to implement a given definition in a trait like this:
```scala
given T = deferred
```
~~`deferred` is a soft keyword which has special meaning only in this context.
A given with `deferred` right hand side can appear only as a member definition of some trait.~~
`deferred` is a new method in the `scala.compiletime` package, which can appear only as the right hand side of a given defined in a trait. Any class implementing that trait will provide an implementation of this given. If a definition is not provided explicitly, it will be synthesized by searching for a given of type `T` in the scope of the inheriting class. Specifically, the scope in which this given will be searched is the environment of that class augmented by its parameters but not containing its members (since that would lead to recursive resolutions). If an implementation _is_ provided explicitly, it counts as an override of a concrete definition and needs an `override` modifier.

Deferred givens allow a clean implementation of context bounds in traits,
as in the following example:
```scala
trait Sorted:
  type Element : Ord

class SortedSet[A : Ord] extends Sorted:
  type Element = A
```
The compiler expands this to the following implementation:
```scala
trait Sorted:
  type Element
  given Ord[Element] = compiletime.deferred

class SortedSet[A](using A: Ord[A]) extends Sorted:
  type Element = A
  override given Ord[Element] = A // i.e. the A defined by the using clause
```

The using clause in class `SortedSet` provides an implementation for the deferred given in trait `Sorted`.

**Benefits:**

 - Better orthogonality, type parameters and abstract type members now accept the same kinds of bounds.
 - Better ergonomics, since deferred givens get naturally implemented in inheriting classes, no need for boilerplate to fill in definitions of abstract givens.

**Alternative:** It was suggested that we use a modifier for a deferred given instead of a `= deferred`. Something like `deferred given C[T]`. But a modifier does not suggest the concept that a deferred given will be implemented automatically in subclasses unless an explicit definition is written. In a sense, we can see `= deferred` as the invocation of a magic macro that is provided by the compiler. So from a user's point of view a given with `deferred` right hand side is not abstract.
It is a concrete definition where the compiler will provide the correct implementation.

## 3. Cleanup of Given Syntax

A good language syntax is like a Bach fugue: A small set of motifs is combined in a multitude of harmonic ways. Dissonances and irregularities should be avoided.

When designing Scala 3, I believe that, by and large, we achieved that goal, except in one area, which is the syntax of givens. There _are_ some glaring dissonances, as seen in this code for defining an ordering on lists:
```scala
given [A](using Ord[A]): Ord[List[A]] with
  def compare(x: List[A], y: List[A]) = ...
```
The `:` feels utterly foreign in this position. It's definitely not a type ascription, so what is its role? Just as bad is the trailing `with`. Everywhere else we use braces or trailing `:` to start a scope of nested definitions, so the need of `with` sticks out like a sore thumb.

We arrived at that syntax not because of a flight of fancy but because even after trying for about a year to find other solutions it seemed like the least bad alternative. The awkwardness of the given syntax arose because we insisted that givens could be named or anonymous, with the default on anonymous, that we would not use underscore for an anonymous given, and that the name, if present, had to come first, and have the form `name [parameters] :`. In retrospect, that last requirement showed a lack of creativity on our part.

Sometimes unconventional syntax grows on you and becomes natural after a while. But here it was unfortunately the opposite. The longer I used given definitions in this style the more awkward they felt, in particular since the rest of the language seemed so much better put together by comparison. And I believe many others agree with me on this. Since the current syntax is unnatural and esoteric, this means it's difficult to discover and very foreign even after that. This makes it much harder to learn and apply givens than it need be.

### New Given Syntax

Things become much simpler if we introduce the optional name instead with an `as name` clause at the end, just like we did for context bounds. We can then use a more intuitive syntax for givens like this:
```scala
given Ord[String]:
  def compare(x: String, y: String) = ...

given [A : Ord] => Ord[List[A]]:
  def compare(x: List[A], y: List[A]) = ...

given Monoid[Int]:
  extension (x: Int) def combine(y: Int) = x + y
  def unit = 0
```
If explicit names are desired, we add them with `as` clauses:
```scala
given Ord[String] as stringOrd:
  def compare(x: String, y: String) = ...

given [A : Ord] => Ord[List[A]] as listOrd:
  def compare(x: List[A], y: List[A]) = ...

given Monoid[Int] as intMonoid:
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

 - Since there is no longer a middle `:` separating name and parameters from the implemented type, we can use a `:` to start the class body without looking unnatural, as is done everywhere else. That eliminates the special case where `with` was used before.

This will be a fairly significant change to the given syntax. I believe there's still a possibility to do this. Not so much code has migrated to new style givens yet, and code that was written can be changed fairly easily. Specifically, there are about a 900K definitions of `implicit def`s
in Scala code on Github and about 10K definitions of `given ... with`. So about 1% of all code uses the Scala 3 syntax, which would have to be changed again.

Changing something introduced just recently in Scala 3 is not fun,
but I believe these adjustments are preferable to let bad syntax
sit there and fester. The cost of changing should be amortized by improved developer experience over time, and better syntax would also help in migrating Scala 2 style implicits to Scala 3. But we should do it quickly before a lot more code
starts migrating.

Migration to the new syntax is straightforward, and can be supported by automatic rewrites. For a transition period we can support both the old and the new syntax. It would be a good idea to backport the new given syntax to the LTS version of Scala so that code written in this version can already use it. The current LTS would then support old and new-style givens indefinitely, whereas new Scala 3.x versions would phase out the old syntax over time.


### Abolish Abstract Givens

Another simplification is possible. So far we have special syntax for abstract givens:
```scala
given x: T
```
The problem is that this syntax clashes with the quite common case where we want to establish a given without any nested definitions. For instance
consider a given that constructs a type tag:
```scala
class Tag[T]
```
Then this works:
```scala
given Tag[String]()
given Tag[String] with {}
```
But the following more natural syntax fails:
```scala
given Tag[String]
```
The last line gives a rather cryptic error:
```
1 |given Tag[String]
  |                 ^
  |                 anonymous given cannot be abstract
```
The problem is that the compiler thinks that the last given is intended to be abstract, and complains since abstract givens need to be named. This is another annoying dissonance. Nowhere else in Scala's syntax does adding a
`()` argument to a class cause a drastic change in meaning. And it's also a violation of the principle that it should be possible to define all givens without providing names for them.

Fortunately, abstract givens are no longer necessary since they are superseded by the new `deferred` scheme. So we can deprecate that syntax over time. Abstract givens are a highly specialized mechanism with a so far non-obvious syntax. We have seen that this syntax clashes with reasonable expectations of Scala programmers. My estimate is that maybe a dozen people world-wide have used abstract givens in anger so far.

**Proposal** In the future, let the `= deferred` mechanism be the only way to deliver the functionality of abstract givens.

This is less of a disruption than it might appear at first:

 - `given T` was illegal before since abstract givens could not be anonymous.
   It now means a concrete given of class `T` with no member definitions.
 - `given x: T` is legacy syntax for an abstract given.
 - `given T as x = deferred` is the analogous new syntax, which is more powerful since
    it allows for automatic instantiation.
 - `given T = deferred` is the anonymous version in the new syntax, which was not expressible before.

**Benefits:**

 - Simplification of the language since a feature is dropped
 - Eliminate non-obvious and misleading syntax.

## Summary of Syntax Changes

Here is the complete context-free syntax for all proposed features.
Overall the syntax for givens becomes a lot simpler than what it was before.

```
TmplDef           ::=  'given' GivenDef
GivenDef          ::=  [GivenConditional '=>'] GivenSig
GivenConditional  ::=  [DefTypeParamClause | UsingParamClause] {UsingParamClause}
GivenSig          ::=  GivenType ['as' id] ([‘=’ Expr] | TemplateBody)
                   |   ConstrApps ['as' id] TemplateBody
GivenType         ::=  AnnotType {id [nl] AnnotType}

TypeDef           ::=  id [TypeParamClause] TypeAndCtxBounds
TypeParamBounds   ::=  TypeAndCtxBounds
TypeAndCtxBounds  ::=  TypeBounds [‘:’ ContextBounds]
ContextBounds     ::=  ContextBound | '{' ContextBound {',' ContextBound} '}'
ContextBound      ::=  Type ['as' id]
```

## Summary

The proposed set of changes removes awkward syntax and makes dealing with context bounds and givens a lot more regular and pleasant. In summary, the proposed changes are:

 1. Allow to name context bounds with `as` clauses.
 2. Give useful default names to other context bounds.
 3. Introduce a less cryptic syntax for multiple context bounds.
 4. Allow context bounds on type members which expand to deferred givens.
 5. Introduce a more regular and clearer syntax for givens.
 6. Eliminate abstract givens.

These changes were implemented as part of a  [draft PR](https://github.com/lampepfl/dotty/pulls/odersky)
which also covers the other prospective changes slated to be proposed in two future SIPs. The new system has proven to work well and to address several fundamental issues people were having with
existing implementation techniques for type classes.

The changes proposed in this pre-SIP are time-sensitive since we would like to correct some awkward syntax choices in Scala 3 before more code migrates to the new constructs (so far, it seems most code still uses Scala 2 style implicits, which will eventually be phased out). It is easy to migrate to the new syntax and to support both old and new for a transition period.
