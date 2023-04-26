class S1a extends J1 {
  override def foo1(x: String | Null): Unit = ???
  override def foo2(): String | Null = ???
}

class S1b extends J1 {
  override def foo1(x: String): Unit = ???
  override def foo2(): String = ???
}