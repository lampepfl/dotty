// Test that we can compare values of regular classes against null,
// but not values of value classes.

class Foo(x: Int)
class Bar(x: Int) extends AnyVal

class Test {
  locally {
    val foo: Foo = new Foo(15)
    foo == null
    null == foo
    foo != null
    null != foo

    val foo2: Foo | Null = foo
    // ok
    foo2 == null
    null == foo2
    foo2 != null
    null != foo2
  }

  locally {
    val bar: Bar = new Bar(15)
    bar == null // error: Values of types Null and Foo cannot be compared
    null == bar // error
    bar != null // error
    null != bar // error

    // To test against null, make the type nullable.
    val bar2: Bar | Null = bar
    // ok
    bar2 == null
    null == bar2
    bar2 != null
    null != bar2
  }
}
