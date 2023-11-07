@main def Test: Unit =
    println(testExpr([B] => (x : B, y : B) => (x, y)))
    println(testExpr([B <: Iterable[Int]] => (x : B) => x))
    println(testExpr([B <: List[Int]] => (x : B) => x))
