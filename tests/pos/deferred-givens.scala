//> using options -language:experimental.modularity -source future
import compiletime.*
class Ord[Elem]
given Ord[Double]

trait A:
  type Elem : Ord
  def foo = summon[Ord[Elem]]

class AC extends A:
  type Elem = Double
  override given Ord[Elem] = ???

class AD extends A:
  type Elem = Double

trait B:
  type Elem
  given Ord[Elem] = deferred
  def foo = summon[Ord[Elem]]

class C extends B:
  type Elem = String
  override given Ord[Elem] = ???

def bar(using Ord[String]) = 1

class D(using Ord[String]) extends B:
  type Elem = String

class E(using x: Ord[String]) extends B:
  type Elem = String
  override given Ord[Elem] = x

class F[X: Ord] extends B:
  type Elem = X
