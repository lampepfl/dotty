@main def Test: Unit =
    println(testExpr([B] => (x : Int, y : Int) => x + y)) // Should match case 2
    println(testExpr([B] => (x : B, y : B) => x)) // Should match case 3
    println(testExpr([B] => (x : B, y : B) => (y, x))) // Should match case 4
    println(testExpr([C, D] => (x : C, f : C => D) => f(x))) // Should match case 4
    println(testExpr([B] => (x : List[B], y : B) => x.indexOf(y))) // Should match case 7
    println(testExpr([B] => (x : B) => [C] => (y : C) => (x, y))) // Should match case 8
    println(testExpr([C, D] => (x : Map[C, D], y: C) => x.get(y)))
