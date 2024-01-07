//> using options -language:experimental.modularity -source future
import hylo.*
import hylo.given

class CollectionTests extends munit.FunSuite:

  test("isEmpty"):
    val empty = AnyCollection(HyArray[Int]())
    assert(empty.isEmpty)

    val nonEmpty = AnyCollection(HyArray[Int](1, 2))
    assert(!nonEmpty.isEmpty)

  test("count"):
    val a = AnyCollection(HyArray[Int](1, 2))
    assertEquals(a.count, 2)

  test("isBefore"):
    val empty = AnyCollection(HyArray[Int]())
    assert(!empty.isBefore(empty.startPosition, empty.endPosition))

    val nonEmpty = AnyCollection(HyArray[Int](1, 2))
    val p0 = nonEmpty.startPosition
    val p1 = nonEmpty.positionAfter(p0)
    val p2 = nonEmpty.positionAfter(p1)
    assert(nonEmpty.isBefore(p0, nonEmpty.endPosition))
    assert(nonEmpty.isBefore(p1, nonEmpty.endPosition))
    assert(!nonEmpty.isBefore(p2, nonEmpty.endPosition))

  test("headAndTail"):
    val empty = AnyCollection(HyArray[Int]())
    assertEquals(empty.headAndTail, None)

    val one = AnyCollection(HyArray[Int](1))
    val Some((h0, t0)) = one.headAndTail: @unchecked
    assert(h0 eq 1)
    assert(t0.isEmpty)

    val two = AnyCollection(HyArray[Int](1, 2))
    val Some((h1, t1)) = two.headAndTail: @unchecked
    assertEquals(h1, 1)
    assertEquals(t1.count, 1)

  test("reduce"):
    val empty = AnyCollection(HyArray[Int]())
    assertEquals(empty.reduce(0)((s, x) => s + x), 0)

    val nonEmpty = AnyCollection(HyArray[Int](1, 2, 3))
    assertEquals(nonEmpty.reduce(0)((s, x) => s + x), 6)

  test("forEach"):
    val empty = AnyCollection(HyArray[Int]())
    assert(empty.forEach((e) => false))

    val nonEmpty = AnyCollection(HyArray[Int](1, 2, 3))
    var s = 0
    assert(nonEmpty.forEach((e) => { s += e; true }))
    assertEquals(s, 6)

    s = 0
    assert(!nonEmpty.forEach((e) => { s += e; false }))
    assertEquals(s, 1)

  test("elementsEqual"):
    val a = HyArray(1, 2)
    assert(a.elementsEqual(a))
end CollectionTests
