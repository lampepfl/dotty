package dotty.tools
package dotc
package printing

import scala.collection.mutable
import core.Contexts._

object Highlighting {

  sealed abstract class Highlight(private val highlight: String) {
    def text: String

    def show(using Context): String = if ctx.useColors then highlight + text + Console.RESET else text

    override def toString: String =
      highlight + text + Console.RESET

    def +(other: Highlight)(using Context): HighlightBuffer =
      new HighlightBuffer(this) + other

    def +(other: String)(using Context): HighlightBuffer =
      new HighlightBuffer(this) + other
  }

  abstract class Modifier(private val mod: String, text: String) extends Highlight(Console.RESET) {
    override def show(using Context): String =
      if (ctx.settings.color.value == "never") ""
      else mod + super.show
  }

  case class HighlightBuffer(hl: Highlight)(using Context) {
    private val buffer = new mutable.ListBuffer[String]

    buffer += hl.show

    def +(other: Highlight): HighlightBuffer = {
      buffer += other.show
      this
    }

    def +(other: String): HighlightBuffer = {
      buffer += other
      this
    }

    override def toString: String =
      buffer.mkString
  }

  case class NoColor(text: String) extends Highlight(Console.RESET)

  case class Red(text: String) extends Highlight(Console.RED)
  case class Blue(text: String) extends Highlight(Console.BLUE)
  case class Cyan(text: String) extends Highlight(Console.CYAN)
  case class Black(text: String) extends Highlight(Console.BLACK)
  case class Green(text: String) extends Highlight(Console.GREEN)
  case class White(text: String) extends Highlight(Console.WHITE)
  case class Yellow(text: String) extends Highlight(Console.YELLOW)
  case class Magenta(text: String) extends Highlight(Console.MAGENTA)

  case class RedB(text: String) extends Highlight(Console.RED_B)
  case class BlueB(text: String) extends Highlight(Console.BLUE_B)
  case class CyanB(text: String) extends Highlight(Console.CYAN_B)
  case class BlackB(text: String) extends Highlight(Console.BLACK_B)
  case class GreenB(text: String) extends Highlight(Console.GREEN_B)
  case class WhiteB(text: String) extends Highlight(Console.WHITE_B)
  case class YellowB(text: String) extends Highlight(Console.YELLOW_B)
  case class MagentaB(text: String) extends Highlight(Console.MAGENTA_B)

  case class Bold(text: String) extends Modifier(Console.BOLD, text)
  case class Underlined(text: String) extends Modifier(Console.UNDERLINED, text)
}
