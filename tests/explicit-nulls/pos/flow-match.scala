// Test flow-typing when NotNullInfos are from cases

object MatchTest {

  def lengthOfStringOrNull(s: String | Null): Int = s match {
    case _: String => s.length
    case _ => 0
  }

  def stringOrNullToString(s: String | Null): String = s match {
    case null => "null"
    // after the null case, s becomes non-nullable
    case _ => s
  }

  def f(s: String | Null): String = s match {
    case null => "other"
    case s2 => s2
    case s3 => s3
  }

  class Foo

  def f2(s: String | Null): String = s match {
    case n @ null => "other"
    case s2 => s2
    case s3 => s3
  }

  def f3(s: String | Null): String = s match {
    case null | "foo" => "other"
    case s2 => s2
    case s3 => s3
  }

  def f4(s: String | Null): String = s match {
    case _ => "other"
    case s2 => s2
    case s3 => s3
  }

  def f5(s: String | Null): String = s match {
    case x => "other"
    case s2 => s2
    case s3 => s3
  }

  def f6(s: String | Null): String = s match {
    case s3: String => s3
    case null => "other"
    case s4 => s4
  }

  def f7(s: String | Null): String = s match {
    case s2 => s2.nn
    case s3 => s3
  }
}
