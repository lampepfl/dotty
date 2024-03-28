// cycle/Cycle.scala
package cycle:
  import scala.concurrent.Future

  object Cycle:
    given heyInt: Future[Int] = ???

// cycle/package.scala
package object cycle:
  export Cycle.heyInt
