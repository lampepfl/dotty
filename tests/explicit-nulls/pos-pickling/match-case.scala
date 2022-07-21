import scala.language.unsafeNulls

def a(): String | Null = ???
val b: String | Null = ???

val i: Int = ???

def f1 = i match {
  case 0 => b
  case _ => a()
}

def f2 = i match {
  case 0 => a()
  case _ => b
}

def f3 = i match {
  case 0 => a()
  case _ => "".trim
}

def f4 = i match {
  case 0 => b
  case _ => "".trim
}

def g1 = i match {
  case 0 => a()
  case 1 => ""
  case _ => null
}

def g2 = i match {
  case 0 => ""
  case 1 => null
  case _ => b
}

def g3 = i match {
  case 0 => null
  case 1 => b
  case _ => ""
}

def h1(i: Int) = i match
  case 0 => 0
  case 1 => true
  case 2 => i.toString
  case _ => null

// This test still fails.
// Even without explicit nulls, the type of Match
// is (0, true, "2"), which is wrong.
// def h2(i: Int) = i match
//   case 0 => 0
//   case 1 => true
//   case 2 => "2"
//   case _ => null