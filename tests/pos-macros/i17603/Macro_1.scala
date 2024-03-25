import scala.quoted.{Quotes, Expr}

case class Location(path: String, line: Int)

object Macros:
  inline given location: Location = ${locationImpl}

  private def checkEquals[T](obtained: T, expected: T) =
    assert(obtained == expected, s"obtained: ${obtained}, expected ${expected}")

  private def locationImpl(using quotes: Quotes): Expr[Location] =
    import quotes.reflect.Position
    val file = Expr(Position.ofMacroExpansion.sourceFile.jpath.toString)
    val line = Expr(Position.ofMacroExpansion.startLine + 1)
    checkEquals(Position.ofMacroExpansion.startLine, 8)
    checkEquals(Position.ofMacroExpansion.sourceFile.jpath.getFileName().toString, "Test_2.scala")
    '{Location($file, $line)}
