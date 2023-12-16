class S {
  val s1: String | Null = ???
  val s2: String = ???
  val n: Null = ???
  val ss1: Array[String] = ???
  val ss2: Array[String | Null] = ???

  locally {
    s1 == null
    s1 != null
    null == s1
    null != s1

    s2 == null
    s2 != null
    null == s2
    null != s2

    s1 == s2
    s1 != s2
    s2 == s1
    s2 != s1

    n == null
    n != null
    null == n
    null != n

    s1 == n
    s2 == n
    n != s1
    n != s2
  }

  locally {
    ss1 == null
    ss1 != null
    null == ss1
    null != ss1

    ss1 == n
    ss1 != n
    n == ss1
    n != ss1

    ss1 == ss2
    ss2 != ss1
  }
}