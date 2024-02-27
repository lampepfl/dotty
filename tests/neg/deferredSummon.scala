//> using options -language:experimental.modularity

object Test:
  given Int = compiletime.deferred // error

abstract class C:
  given Int = compiletime.deferred // error

trait A:
  import compiletime.deferred
  locally:
    given Int = deferred // error

trait B:
  import compiletime.deferred as defered
  given Int = defered // error



