package dotty.tools.dotc
package core
package tasty

class TastyAnsiiPrinter(bytes: Array[Byte], testPickler: Boolean, isBestEffortTasty: Boolean = false) extends TastyPrinter(bytes, testPickler, isBestEffortTasty) {

  def this(bytes: Array[Byte]) = this(bytes, testPickler = false)

  override protected def nameStr(str: String): String = Console.MAGENTA + str + Console.RESET
  override protected def treeStr(str: String): String = Console.YELLOW + str + Console.RESET
  override protected def lengthStr(str: String): String = Console.CYAN + str + Console.RESET
}
