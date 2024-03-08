import scala.language.experimental.clauseInterleaving

object Test extends App {
  trait Key { type Value }
  trait DB {
    def getOrElse(k: Key)[V >: k.Value](default: V): V // dependent type parameter
  }

  val key1 = new Key{ type Value = Some[Int] }
  val key2 = new Key{ type Value = Some[Int] }
  val key3 = new Key{ type Value = Some[String] }

  val db1: DB = new DB{
    def getOrElse(k: Key)[V >: k.Value](default: V): V = if k == key1 then Some(4).asInstanceOf[k.Value] else default
  }

  // Interleaved method with dependent type bound
  val default1: None.type = None
  assert(db1.getOrElse(key1)[Option[Int]](default1) == Some(4))
  assert(db1.getOrElse(key2)[Option[Int]](default1) == default1)
  assert(db1.getOrElse(key3)[Option[String]](default1) == default1)
  assert(db1.getOrElse(key1)(default1) == Some(4))
  assert(db1.getOrElse(key2)(default1) == default1)
  assert(db1.getOrElse(key3)(default1) == default1)

  val default2: Any = 3
  assert(db1.getOrElse(key1)[Any](default2) == Some(4))
  assert(db1.getOrElse(key2)[Any](default2) == default2)
  assert(db1.getOrElse(key3)[Any](default2) == default2)
  assert(db1.getOrElse(key1)(default2) == Some(4))
  assert(db1.getOrElse(key2)(default2) == default2)
  assert(db1.getOrElse(key3)(default2) == default2)

  // Extension method and using parameter
  extension (k: Key)
    def lookupOrElse(using db: DB)[V >: k.Value](default: V): V = db.getOrElse(k)(default)

  object Block1:
    given DB = db1

    assert(key1.lookupOrElse[Option[Int]](default1) == Some(4))
    assert(key2.lookupOrElse[Option[Int]](default1) == default1)
    assert(key3.lookupOrElse[Option[String]](default1) == default1)
    assert(key1.lookupOrElse(default1) == Some(4))
    assert(key2.lookupOrElse(default1) == default1)
    assert(key3.lookupOrElse(default1) == default1)

    assert(key1.lookupOrElse[Any](default2) == Some(4))
    assert(key2.lookupOrElse[Any](default2) == default2)
    assert(key3.lookupOrElse[Any](default2) == default2)
    assert(key1.lookupOrElse(default2) == Some(4))
    assert(key2.lookupOrElse(default2) == default2)
    assert(key3.lookupOrElse(default2) == default2)
  end Block1

  // Right associative extension method
  extension (db: DB)
    def ?:(k: Key)[V >: k.Value](default: V): V = db.getOrElse(k)(default)

  assert((db1 ?: (key1))[Option[Int]](default1) == Some(4))
  assert((db1 ?: (key2))[Option[Int]](default1) == default1)
  assert((db1 ?: (key3))[Option[String]](default1) == default1)
  assert((db1 ?: (key1))(default1) == Some(4))
  assert((db1 ?: (key2))(default1) == default1)
  assert((db1 ?: (key3))(default1) == default1)

  assert((db1 ?: (key1))[Any](default2) == Some(4))
  assert((db1 ?: (key2))[Any](default2) == default2)
  assert((db1 ?: (key3))[Any](default2) == default2)
  assert((db1 ?: (key1))(default2) == Some(4))
  assert((db1 ?: (key2))(default2) == default2)
  assert((db1 ?: (key3))(default2) == default2)


  assert(key1.?:(db1)[Option[Int]](default1) == Some(4))
  assert(key2.?:(db1)[Option[Int]](default1) == default1)
  assert(key3.?:(db1)[Option[String]](default1) == default1)
  assert(key1.?:(db1)(default1) == Some(4))
  assert(key2.?:(db1)(default1) == default1)
  assert(key3.?:(db1)(default1) == default1)

  assert(key1.?:(db1)[Any](default2) == Some(4))
  assert(key2.?:(db1)[Any](default2) == default2)
  assert(key3.?:(db1)[Any](default2) == default2)
  assert(key1.?:(db1)(default2) == Some(4))
  assert(key2.?:(db1)(default2) == default2)
  assert(key3.?:(db1)(default2) == default2)


  assert(?:(key1)(db1)[Option[Int]](default1) == Some(4))
  assert(?:(key2)(db1)[Option[Int]](default1) == default1)
  assert(?:(key3)(db1)[Option[String]](default1) == default1)
  assert(?:(key1)(db1)(default1) == Some(4))
  assert(?:(key2)(db1)(default1) == default1)
  assert(?:(key3)(db1)(default1) == default1)

  assert(?:(key1)(db1)[Any](default2) == Some(4))
  assert(?:(key2)(db1)[Any](default2) == default2)
  assert(?:(key3)(db1)[Any](default2) == default2)
  assert(?:(key1)(db1)(default2) == Some(4))
  assert(?:(key2)(db1)(default2) == default2)
  assert(?:(key3)(db1)(default2) == default2)
}
