
class S {
  
  val j = new J()
  j.foo(null) // ok: argument is nullable
  val s: String = j.foo("hello") // error: return type is nullable
  
  J.fooStatic(null) // ok: argument is nullable
  val s2: String = J.fooStatic("hello") // error: return type is nullable
}
