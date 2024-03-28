type A = Any { var x: Int }

val f: Any { var i: Int } = new AnyRef { var i: Int = 0 }

def Test =
  summon[Any { def x: Int; def x_=(x: Int): Unit } <:< Any { var x: Int }]
  summon[Any { var x: Int } <:< Any { def x: Int; def x_=(x: Int): Unit }]
