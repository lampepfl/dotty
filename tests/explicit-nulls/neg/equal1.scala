// Test what can be compared for equality against null.

case class VC(x: Int) extends AnyVal

def test =
  // Null itself
  val x0: Null = null
  x0 != x0
  x0 == null
  x0 != null
  null == x0
  null == null
  null != null

  // Non-nullable types: OK.
  val x1: String = "hello"
  x1 != null
  x1 == null
  null == x1
  null != x1
  x1 == x0
  x0 != x1
  x1.asInstanceOf[String | Null] == null
  x1.asInstanceOf[String | Null] == x0
  x1.asInstanceOf[Any] == null
  x1.asInstanceOf[Any] == x0

  // Nullable types: OK
  val x2: String | Null = null
  x2 == null
  null == x2
  x2 == x0
  x2 != x0
  x0 == x2
  x2 == x1
  x2 != x1
  x1 == x2

  // Value types: not allowed.
  1 == null     // error
  null != 0     // error
  null == 0     // error
  true == null  // error
  null == false // error
  'a' == null   // error
  null == 'b'   // error

  // Nullable value types: OK.
  val x3: Int | Null = null
  x3 == null
  null == x3
  x3 == x0
  x3 != x0
