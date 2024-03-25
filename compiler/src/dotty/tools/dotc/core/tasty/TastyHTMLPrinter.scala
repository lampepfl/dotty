package dotty.tools.dotc
package core
package tasty

class TastyHTMLPrinter(bytes: Array[Byte]) extends TastyPrinter(bytes, isBestEffortTasty = false, testPickler = false) {
  override protected def nameStr(str: String): String = s"<span class='name'>$str</span>"
  override protected def treeStr(str: String): String = s"<span class='tree'>$str</span>"
  override protected def lengthStr(str: String): String = s"<span class='length'>$str</span>"
}
