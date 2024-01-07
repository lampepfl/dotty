//> using options -language:experimental.modularity -source future
import hylo.*
import hylo.given

class IntegersTests extends munit.FunSuite:

    test("Int.hashInto"):
        val x = Hasher.hash(42)
        val y = Hasher.hash(42)
        assertEquals(x, y)

        val z = Hasher.hash(1337)
        assertNotEquals(x, z)

