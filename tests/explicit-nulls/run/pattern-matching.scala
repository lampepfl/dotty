object Test:

  def main(args: Array[String]): Unit =

    val s: String = null.asInstanceOf[String]

    val r1 = s match
      case s: String => 100
      case _ => 200
    assert(r1 == 200)

    val r2 = s match
      case s: String => 100
      case null => 200
    assert(r2 == 200)

    val r3 = s match
      case null => 100
      case _ => 200
    assert(r3 == 100)

    val s2: String | Null = null

    val r4 = s2 match
      case s2: String => 100
      case _ => 200
    assert(r4 == 200)

    val r5 = s2 match
      case s2: String => 100
      case null => 200
    assert(r5 == 200)

    val r6 = s2 match
      case null => 200
      case s2: String => 100
    assert(r6 == 200)