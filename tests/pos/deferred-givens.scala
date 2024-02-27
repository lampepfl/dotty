//> using options -language:experimental.modularity -source future
import compiletime.*
class Ord[Elem]

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
