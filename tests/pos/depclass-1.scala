//> using options -source future -language:experimental.modularity
class A(tracked val source: String)

class B(x: Int, tracked val source1: String) extends A(source1)

class C(tracked val source2: String) extends B(1, source2)

//class D(source1: String) extends C(source1)
val x = C("hello")
val _: A{ val source: "hello" } = x

class Vec[Elem](tracked val size: Int)
class Vec8 extends Vec[Float](8)

val v = Vec[Float](10)
val v2 = Vec8()
val xx: 10 = v.size
val x2: 8 = v2.size

