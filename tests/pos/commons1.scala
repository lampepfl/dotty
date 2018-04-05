/** Simple common definitions, with This type */
trait Text {
  val `common`: Text.Common
  import `common`._

  def length: Int
  def apply(idx: Int): Char
  def concat(txt: Instance): Instance
  def toStr: String
  def flatten = common.fromString(toStr)
}
object Text {
  trait Common { self =>
    type Instance <: Text { val `common`: self.type }
    def fromString(str: String): Instance
    def fromStrings(strs: String*): Instance =
      ("" :: strs.toList).map(fromString).reduceLeft(_.concat(_))
  }
}

class FlatText(str: String) extends Text {
  val common: FlatText.type = FlatText
  def length = str.length
  def apply(n: Int) = str.charAt(n)
  def concat(txt: FlatText) = new FlatText(str ++ txt.toStr)
  def toStr = str
}
object FlatText extends Text.Common {
  type Instance = FlatText
  def fromString(str: String) = new FlatText(str)
}

enum ConcText extends Text {
  val common: ConcText.type = ConcText

  case Str(s: String)
  case Conc(t1: Text, t2: Text)

  lazy val length = this match {
    case Str(s) => s.length
    case Conc(t1, t2) => t1.length + t2.length
  }

  def apply(n: Int) = this match {
    case Str(s) => s.charAt(n)
    case Conc(t1, t2) => if (n < t1.length) t1(n) else t2(n - t1.length)
  }

  def concat(txt: ConcText): ConcText = Conc(this, txt)

  def toStr: String = this match {
    case Str(s) => s
    case Conc(t1, t2) => t1.toStr ++ t2.toStr
  }
}

object ConcText extends Text.Common {
  type Instance = ConcText
  def fromString(str: String) = Str(str)
}

object Test extends App {
  val txt1 = ConcText.fromStrings("hel", "lo")
  val txt2 = ConcText.fromString("world")
  println(txt2.concat(txt1))
  assert(txt1.concat(txt2).toStr == "helloworld")
  assert(txt2.concat(txt1).toStr == "worldhello")
  assert(txt1.concat(txt2)(5) == 'w')
}