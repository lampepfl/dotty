package dotty.tools.dotc.config

object Printers {

  class Printer {
    def println(msg: => String): Unit = System.out.nn.println(msg)
  }

  object noPrinter extends Printer {
    inline override def println(msg: => String): Unit = ()
  }

  val default = new Printer

  val capt = noPrinter
  val constr = noPrinter
  val core = noPrinter
  val checks = noPrinter
  val config = noPrinter
  val cyclicErrors = noPrinter
  val debug = noPrinter
  val derive = noPrinter
  val desugar = noPrinter
  val scaladoc = noPrinter
  val exhaustivity = noPrinter
  val gadts = noPrinter
  val gadtsConstr = noPrinter
  val hk = noPrinter
  val implicits = noPrinter
  val implicitsDetailed = noPrinter
  val lexical = noPrinter
  val init = noPrinter
  val inlining = noPrinter
  val interactiv = noPrinter
  val macroAnnot = noPrinter
  val matchTypes = noPrinter
  val nullables = noPrinter
  val overload = noPrinter
  val patmatch = noPrinter
  val pickling = noPrinter
  val quotePickling = noPrinter
  val plugins = noPrinter
  val recheckr = noPrinter
  val refcheck = noPrinter
  val simplify = noPrinter
  val staging = noPrinter
  val subtyping = noPrinter
  val tailrec = noPrinter
  val transforms = noPrinter
  val typr = noPrinter
  val unapp = noPrinter
  val variances = noPrinter
  val saferExceptions = noPrinter
}
