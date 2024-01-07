//> using options -language:experimental.modularity

object Test:
  given Int = deferredSummon // error

abstract class C:
  given Int = deferredSummon // error

trait A:
  locally:
    given Int = deferredSummon // error

