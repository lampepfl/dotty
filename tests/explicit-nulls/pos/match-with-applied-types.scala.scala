class A

def test =
  val xs: java.util.LinkedHashMap[String, A | List[A]] = ???
  xs.get("a") match
    case a: A => ???
    case as: List[A] => ???