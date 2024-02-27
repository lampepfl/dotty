import compiletime.*
class Ord[Elem]

trait B:
  type Elem
  given Ord[Elem] = deferred
  def foo = summon[Ord[Elem]]

class C extends B:
  type Elem = String
  given Ord[Elem] = ???
