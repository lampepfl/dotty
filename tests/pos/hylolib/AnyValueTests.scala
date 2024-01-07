//> using options -language:experimental.modularity -source future
import hylo.*
import hylo.given

class AnyValueTests extends munit.FunSuite:

  test("eq"):
    val a = AnyValue(1)
    assert(a `eq` a)
    assert(!(a `neq` a))

    val b = AnyValue(2)
    assert(!(a `eq` b))
    assert(a `neq` b)

