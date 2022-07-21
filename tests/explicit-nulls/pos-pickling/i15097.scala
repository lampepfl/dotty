class C:
  def g: String | Null = ???

  def f =
    import scala.language.unsafeNulls
    try g catch case _ => ""

  def f2 =
    import scala.language.unsafeNulls
    if ??? then g else ""

  def f3 =
    import scala.language.unsafeNulls
    (??? : Boolean) match
    case true => g
    case _ => ""

class C2:
  import scala.language.unsafeNulls
  def g: String | Null = ???

  def f =
    try g catch case _ => ""

  def f2 =
    if ??? then g else ""

  def f3 =
    (??? : Boolean) match
    case true => g
    case _ => ""
