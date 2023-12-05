import language.experimental.namedTuples

def f: NamedTuple.NamedTuple[(Int, Any), (Int, String)] = ???

type Person = (name: Int, age: String)

val p: Person = f  // error
