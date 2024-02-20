package dotty.tools.backend.sjs

import dotty.tools.dotc.core.*
import Contexts.*
import Phases.*

/** Generates Scala.js IR files for the compilation unit. */
class GenSJSIR extends Phase {

  override def phaseName: String = GenSJSIR.name

  override def description: String = GenSJSIR.description

  override def isRunnable(using Context): Boolean =
    super.isRunnable && ctx.settings.scalajs.value && !ctx.usedBestEffortTasty

  def run(using Context): Unit =
    new JSCodeGen().run()
}

object GenSJSIR:
  val name: String = "genSJSIR"
  val description: String = "generate .sjsir files for Scala.js"
