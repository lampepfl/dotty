import language.experimental.namedTuples

type Person = (name: String, age: Int)

def test =
  val bob = (name = "Bob", age = 33): (name: String, age: Int)

  val silly = bob match
    case (name = n, age = a) => n.length + a
