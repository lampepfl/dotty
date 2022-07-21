import scala.language.unsafeNulls

def associatedFile: String | Null = ???

def source: String = {
  val f = associatedFile
  if (f != null) f else associatedFile
}

def defines(raw: String): List[String] = {
  val ds: List[(Int, Int)] = ???
  ds map { case (start, end) => raw.substring(start, end) }
}

abstract class DeconstructorCommon[T >: Null <: AnyRef] {
  var field: T = null
  def get: this.type = this
  def isEmpty: Boolean = field eq null
  def isDefined = !isEmpty
  def unapply(s: T): this.type ={
    field = s
    this
  }
}

def genBCode =
  val bsmArgs: Array[Object | Null] | Null = null
  val implMethod = bsmArgs(3).asInstanceOf[Integer].toInt
  implMethod

val arrayApply = "a".split(" ")(2)

val globdir: String = if (??? : Boolean) then associatedFile.replaceAll("[\\\\/][^\\\\/]*$", "") else ""

def newInstOfC(c: Class[?]) = c.getConstructor().newInstance()