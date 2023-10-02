---
layout: doc-page
title: "Trait Parameters"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/trait-parameters.html
---

Scala 3 allows traits to have parameters, just like classes have parameters.

```scala
trait Greeting(val name: String):
  def msg = s"How are you, $name"

class C extends Greeting("Bob"):
  println(msg)
```

Arguments to a trait are evaluated immediately before the trait is initialized.

One potential issue with trait parameters is how to prevent
ambiguities. For instance, you might try to extend `Greeting` twice,
with different parameters.

```scala
class D extends C, Greeting("Bill") // error: parameter passed twice
```

Should this print "Bob" or "Bill"? In fact this program is illegal,
because it violates the second rule of the following for trait parameters:

 1. If a class `C` extends a parameterized trait `T`, and its superclass does not, `C` _must_ pass arguments to `T`.

 2. If a class `C` extends a parameterized trait `T`, and its superclass does as well, `C` _must not_  pass arguments to `T`.

 3. Traits must never pass arguments to parent traits.

Here's a trait extending the parameterized trait `Greeting`.

```scala
trait FormalGreeting extends Greeting:
  override def msg = s"How do you do, $name"
```
As is required, no arguments are passed to `Greeting`. However, this poses an issue
when defining a class that extends `FormalGreeting`:

```scala
class E extends FormalGreeting // error: missing arguments for `Greeting`.
```

The correct way to write `E` is to extend both `Greeting` and
`FormalGreeting` (in either order):

```scala
class E extends Greeting("Bob"), FormalGreeting
```

## Default values and implicit parameters

If the trait `T` is parametrised such that `new T{}` would be a legal annonymous class creation, the "explicit extension required" rule does not apply, the parameters filled in as for the annonymous class:
* Parameters with default values are set to their default value.
* [Context parameters](../contextual/using-clauses.md) are set to the value present in the implicit scope. Notably from class parameters or other traits.

```scala
trait withLastName(val lastName: String = "")

case class Person() extends withLastName
// identical to
case class Person extends withLastName("")
```

```scala
case class ImpliedName(name: String):
  override def toString = name

trait ImpliedGreeting(using val iname: ImpliedName):
  def msg = s"How are you, $iname"

trait ImpliedFormalGreeting extends ImpliedGreeting:
  override def msg = s"How do you do, $iname"

class F(using iname: ImpliedName) extends ImpliedFormalGreeting
```

The definition of `F` in the last line is implicitly expanded to
```scala
class F(using iname: ImpliedName) extends
  Object,
  ImpliedGreeting(using iname),
  ImpliedFormalGreeting
```
Note the inserted reference to the super trait `ImpliedGreeting`, which was not mentioned explicitly.

In the above context, the following is also valid:
```scala
given iname: ImpliedName
class F2() extends ImpliedFormalGreeting
```
And expands to:
```scala
class F2() extends
  Object,
  ImpliedGreeting(using iname),
  ImpliedFormalGreeting
```

Forms of ommission can be combined:
```scala
trait SuperTrait(using Char)(val name: String = "")(using Int)

given c: Char = 'X'
class SuperClass(using i: Int) extends SuperTrait
```
The last line expands to:

```scala
class SuperClass(using i: Int) extends SuperTrait(using c)("")(using i)
```

## Reference

For more information, see [Scala SIP 25](http://docs.scala-lang.org/sips/pending/trait-parameters.html).
TODO: Find which PR changed the behaviour, as this was changed between 3.2.2 and 3.2.1
