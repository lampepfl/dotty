//> using options -language:experimental.modularity -source future
import hylo.*
import hylo.given

object munit:
    open class FunSuite:
        def test(name: String)(op: => Unit): Unit = op
        def assertEquals[T](x: T, y: T) = assert(x == y)
        def assertNotEquals[T](x: T, y: T) = assert(x != y)

@main def Test =
    CollectionTests()
    AnyValueTests()
    HyArrayTests()
    IntegersTests()
    println("done")
