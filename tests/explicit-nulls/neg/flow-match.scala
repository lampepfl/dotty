// Test flow-typing when NotNullInfos are from cases

object MatchTest {
  def f6(s: String | Null): String = s match {
    case s2 => s2 // error
    case null => "other"
    case s3 => s3
  }

  def f7(s: String | Null): String = s match {
    case null => "other"
    case null => "other"
    case s3 => s3
  }
}
