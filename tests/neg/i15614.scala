// scalac: -explain
def f(i: Int, using j: Int) = i + j   // error // error

def g(i: Int, using Int) = i + summon[Int]  // error  // error

def z(i: Int, using) = i // error

/*
was unhelpful:
at 2: Not found: j
at 2: ':' expected, but identifier found
*/
