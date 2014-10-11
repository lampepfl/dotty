package dotty.tools.dotc.typer

import collection.mutable

case class Mode(val bits: Int) extends AnyVal {
  import Mode._
  def | (that: Mode) = Mode(bits | that.bits)
  def & (that: Mode) = Mode(bits & that.bits)
  def &~ (that: Mode) = Mode(bits & ~that.bits)
  def is (that: Mode) = (bits & that.bits) == that.bits

  def isExpr = (this & PatternOrType) == None

  override def toString =
    (0 until 31).filter(i => (bits & (1 << i)) != 0).map(modeName).mkString("Mode(", ",", ")")
}

object Mode {
  val None = Mode(0)

  private var modeName = new Array[String](32)

  def newMode(bit: Int, name: String): Mode = {
    modeName(bit) = name
    Mode(1 << bit)
  }

  val Pattern = newMode(0, "Pattern")
  val Type = newMode(1, "Type")

  val ImplicitsEnabled = newMode(2, "ImplicitsEnabled")
  val InferringReturnType = newMode(3, "InferringReturnType")

  val TypevarsMissContext = newMode(4, "TypevarsMissContext")
  val CheckCyclic = newMode(5, "CheckCyclic")

  val InSuperCall = newMode(6, "InSuperCall")

  /** This mode bit is set if we want to allow accessing a symbol's denotation
   *  at a period before that symbol is first valid. An example where this is
   *  the case is if we want to examine the environment where an access is made.
   *  The computation might take place at an earlier phase (e.g. it is part of
   *  some completion such as unpickling), but the environment might contain
   *  synbols that are not yet defined in that phase.
   *  If the mode bit is set, getting the denotation of a symbol at a phase
   *  before the symbol is defined will return the symbol's denotation at the
   *  first phase where it is valid, instead of throwing a NotDefinedHere error.
   */
  val FutureDefsOK = newMode(7, "FutureDefsOK")

  /** Allow GADTFlexType labelled types to have their bounds adjusted */
  val GADTflexible = newMode(8, "GADTflexible")

  val PatternOrType = Pattern | Type
}