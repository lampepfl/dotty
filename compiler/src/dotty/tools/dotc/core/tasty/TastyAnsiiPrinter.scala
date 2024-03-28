package dotty.tools.dotc
package core
package tasty

class TastyAnsiiPrinter(bytes: Array[Byte], isBestEffortTasty: Boolean, testPickler: Boolean) extends TastyPrinter(bytes, isBestEffortTasty, testPickler) {

  def this(bytes: Array[Byte]) = this(bytes, isBestEffortTasty = false, testPickler = false)

  override protected def nameStr(str: String): String = Console.MAGENTA + str + Console.RESET
  override protected def treeStr(str: String): String = Console.YELLOW + str + Console.RESET
  override protected def lengthStr(str: String): String = Console.CYAN + str + Console.RESET
}
