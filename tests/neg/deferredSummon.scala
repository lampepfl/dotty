//> using options -language:experimental.modularity

object Test:
  given Int = deferred // error

abstract class C:
  given Int = deferred // error

trait A:
  locally:
    given Int = deferred // error

