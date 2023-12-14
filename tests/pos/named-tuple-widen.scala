import language.experimental.namedTuples

class A
class B
val y1: (a1: A, b1:  B) = ???
val y2: (a2: A, b2: B) = ???
var z1 = if ??? then y1 else y2 // -- what is the type of z2
var z2: NamedTuple.AnyNamedTuple = z1
val _ = z1 = z2