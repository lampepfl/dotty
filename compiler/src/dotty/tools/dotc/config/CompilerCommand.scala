package dotty.tools.dotc
package config

import Settings.*
import core.Contexts.*

abstract class CompilerCommand extends CliCommand:
  type ConcreteSettings = ScalaSettings
  import ScalaSettings.*

  private val customMessageFlags: Set[Setting[_]] = Set(ScalaSettings.W)
  final def helpMsg(using settings: ConcreteSettings)(using SettingsState, Context): String =
    allSettings.find(isHelping) match
      case Some(s) if !customMessageFlags.contains(s) => 
        s.description
      case _ =>
        if (help.value) usageMessage
        else if (Vhelp.value) vusageMessage
        else if (W.value.contains("help")) wusageMessage
        else if (Xhelp.value) xusageMessage
        else if (Yhelp.value) yusageMessage
        else if (showPlugins.value) ctx.base.pluginDescriptions
        else if (XshowPhases.value) phasesMessage
        else ""

  final def isHelpFlag(using settings: ConcreteSettings)(using SettingsState): Boolean =
    val flags = Set(help, Vhelp, Xhelp, Yhelp, showPlugins, XshowPhases)
    flags.exists(_.value) || allSettings.exists(isHelping)
