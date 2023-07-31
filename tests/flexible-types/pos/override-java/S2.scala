import java.util.List

class S2a extends J2 {
  override def bar1(xs: List[String] | Null): Unit = ???
  override def bar2(xss: List[Array[String | Null]] | Null): Unit = ???

  override def bar3(): List[String] | Null = ???
  override def bar4(): List[Array[String | Null]] | Null = ???
}

class S2b extends J2 {
  override def bar1(xs: List[String]): Unit = ???
  override def bar2(xss: List[Array[String | Null]]): Unit = ???

  override def bar3(): List[String] = ???
  override def bar4(): List[Array[String | Null]] = ???
}

class S2c extends J2 {
  override def bar1(xs: List[String]): Unit = ???
  override def bar2(xss: List[Array[String]]): Unit = ???

  override def bar3(): List[String] = ???
  override def bar4(): List[Array[String]] = ???
}