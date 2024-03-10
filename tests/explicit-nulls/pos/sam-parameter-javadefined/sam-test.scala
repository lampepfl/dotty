def foo = {
  def unit: Unit = ()

  injava.overloaded({ () => unit } : Runnable )
  injava.overloaded({ () => unit } )

  injava.notoverloaded({ () => unit } : Runnable )
  injava.notoverloaded({ () => unit } )

  val list = new java.util.Vector[Int]()
  java.util.Collections.sort[Int](list, { (a,b) => a - b } : java.util.Comparator[Int] )
  java.util.Collections.sort[Int](list, { (a,b) => a - b })

  new Thread({ () => unit } : Runnable )
  new Thread({ () => unit } )

  // See cats.effect.kernel.AsyncPlatform
  val cf = new java.util.concurrent.CompletableFuture[String]
  cf.handle[Unit]({
      case (string, null) => unit
      case (string, throwable) => unit
    })
}
