trait FakeEnum[A, @specialized(Byte, Short, Int, Long) B]
{
  trait Value {
    self: A =>
      def name: String
      def id: B
  }
}

object FakeEnumType
  extends FakeEnum[FakeEnumType, Short]
{
  val MEMBER1 = new FakeEnumType((0: Short), "MEMBER1") {}
  val MEMBER2 = new FakeEnumType((1: Short), "MEMBER2") {}
}

sealed abstract
class FakeEnumType(val id: Short, val name: String)
  extends FakeEnumType.Value
{}
