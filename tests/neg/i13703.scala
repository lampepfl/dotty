trait Foo extends reflect.Selectable

val f2: Foo { val i: Int; def i_=(x: Int): Unit } = new Foo { var i: Int = 0 } // error

val f3: Foo { def i: Int; def i_=(x: Int): Unit } = new Foo { var i: Int = 0 } // OK
