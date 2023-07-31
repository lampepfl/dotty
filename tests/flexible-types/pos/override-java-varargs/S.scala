class S1 extends J {
  override def foo(x: (String | Null)*): Unit = ???
  override def bar(x: String | Null, y: (String | Null)*): Unit = ???
}

class S2 extends J {
  override def foo(x: String*): Unit = ???
  override def bar(x: String | Null, y: String*): Unit = ???
}

class S3 extends J {
  override def foo(x: String*): Unit = ???
  override def bar(x: String, y: String*): Unit = ???
}
