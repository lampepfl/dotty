//> using options -source future -language:experimental.modularity
import coll.*
class Lst8 extends Lst(8)

val v8a: Vec { val size: 8 } = new Vec8
val v8b: Vec { val size: 8 } = new Vec(8) {}

val l8a: Lst { val size: 8 } = new Lst8
val l8b: Lst { val size: 8 } = new Lst(8) {}

class VecN(tracked val n: Int) extends Vec(n)
class Vec9 extends VecN(9)
val v9a = VecN(9)
val _: Vec { val size: 9 } = v9a
val v9b = Vec9()
val _: Vec { val size: 9 } = v9b
