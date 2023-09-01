package dotty.tools.dotc

import dotty.tools.dotc.core.SymDenotations.NoDenotation.exists
import scala.collection.mutable
import core.*
import ast.*
import Symbols.*
import EventLog.*
import Contexts.*
import dotty.tools.dotc.transform.CheckUnused

class EventLog {
  private val entries: mutable.ListBuffer[Entry] = mutable.ListBuffer()
  private var state = 0

  inline def appendEntry(entry: Entry)(using Context): Unit = {
    state match {
      case Disabled => ()
      case Enabled => entries += entry
      case Uninitialized =>
        val enabled = ctx.base.allPhases.exists { phase =>
          logReaderPhasesNames.contains(phase.phaseName) && phase.isRunnable
        }
        state =
          if enabled then
            entries += entry
            Enabled
          else Disabled
    }
  }

  def toSeq: Seq[Entry] = entries.toSeq

}

object EventLog {

  private val logReaderPhasesNames = Set(
    CheckUnused.phaseNamePrefix + CheckUnused.afterTyperSuffix,
    CheckUnused.phaseNamePrefix + CheckUnused.afterInliningSuffix
  )

  private val Uninitialized: Int = 0
  private val Enabled = 1
  private val Disabled = 2

  sealed trait Entry
  case class MatchedImportSelector(importSym: Symbol, selector: untpd.ImportSelector) extends Entry
  case class ImplicitIntroducedViaImport(importSym: Symbol, tree: Tree, ref: TermRef) extends Entry
  case object FindRefFinished extends Entry

}
