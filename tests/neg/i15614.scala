// scalac: -explain
def f(i: Int, using j: Int) = i + j   // error // error

def g(i: Int, using Int) = i + summon[Int]  // error  // error

def z(i: Int, using) = i // error

def erased(i: Int, erased j: Int) = i + j // error // error

def usingAndErased(i: Int, using erased j: Int) = i + j // error // error

def usingAndErasedType(i: Int, using erased Int) = i + j // error // error

def untoken(using: Int) = using // error // error

class C(i: Int, using j: Int): // error
  def c = i + summon[Int] // error

/*
was unhelpful:
at 2: Not found: j
at 2: ':' expected, but identifier found
*/
