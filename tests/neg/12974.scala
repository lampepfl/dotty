package example

object RecMap {

  object Record {
    // use this scope to bound who can see inside the opaque type
    opaque type Rec[A <: Tuple] = Map[String, Any]

    object Rec {
      type HasKey[A <: Tuple, K] =
        A match
          case (K, t) *: _ => t
          case _ *: t => HasKey[t, K]

      val empty: Rec[EmptyTuple] = Map.empty

      extension [A <: Tuple](toMap: Rec[A])
        def fetch[K <: String & Singleton](key: K): HasKey[A, K] =
          toMap(key).asInstanceOf[HasKey[A, K]]
    }
  }

  def main(args: Array[String]) =
    import Record._

    val foo: Any = Rec.empty.fetch("foo") // error
    //                            ^
    // Match type reduction failed since selector  EmptyTuple
    // matches none of the cases
    //
    //     case (("foo" : String), t) *: _ => t
    //     case _ *: t => example.RecMap.Record.Rec.HasKey[t, ("foo" : String)]

  end main
}
