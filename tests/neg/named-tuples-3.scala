import language.experimental.namedTuples

def f: NamedTuple.NamedTuple[(Int, Any), (Int, String)] = ??? // error

type Person = (name: Int, age: String)

val p: Person = f
