package dotty.tools
package dotc
package config

import core._
import Contexts._, Symbols._, Names._, NameOps._, Phases._
import StdNames.nme
import Decorators.{_, given}
import util.SrcPos
import SourceVersion._
import reporting.Message
import NameKinds.QualifiedName

object Feature:

  private def experimental(str: String): TermName =
    QualifiedName(nme.experimental, str.toTermName)

  private def deprecated(str: String): TermName =
    QualifiedName(nme.deprecated, str.toTermName)

  private val Xdependent = experimental("dependent")
  private val XnamedTypeArguments = experimental("namedTypeArguments")
  private val XgenericNumberLiterals = experimental("genericNumberLiterals")
  private val Xmacros = experimental("macros")
  private val symbolLiterals: TermName = deprecated("symbolLiterals")

/** Is `feature` enabled by by a command-line setting? The enabling setting is
   *
   *       -language:<prefix>feature
   *
   *  where <prefix> is the fully qualified name of `owner`, followed by a ".",
   *  but subtracting the prefix `scala.language.` at the front.
   */
  def enabledBySetting(feature: TermName)(using Context): Boolean =
    ctx.base.settings.language.value.contains(feature.toString)

  /** Is `feature` enabled by by an import? This is the case if the feature
   *  is imported by a named import
   *
   *       import owner.feature
   *
   *  and there is no visible nested import that excludes the feature, as in
   *
   *       import owner.{ feature => _ }
   */
  def enabledByImport(feature: TermName)(using Context): Boolean =
    //atPhase(typerPhase) {
      ctx.importInfo != null && ctx.importInfo.featureImported(feature)
    //}

  /** Is `feature` enabled by either a command line setting or an import?
   *  @param  feature   The name of the feature
   *  @param  owner     The prefix symbol (nested in `scala.language`) where the
   *                    feature is defined.
   */
  def enabled(feature: TermName)(using Context): Boolean =
    enabledBySetting(feature) || enabledByImport(feature)

  /** Is auto-tupling enabled? */
  def autoTuplingEnabled(using Context): Boolean = !enabled(nme.noAutoTupling)

  def dynamicsEnabled(using Context): Boolean = enabled(nme.dynamics)

  def dependentEnabled(using Context) = enabled(Xdependent)

  def namedTypeArgsEnabled(using Context) = enabled(XnamedTypeArguments)

  def genericNumberLiteralsEnabled(using Context) = enabled(XgenericNumberLiterals)

  def symbolLiteralsEnabled(using Context) = enabled(symbolLiterals)

  def scala2ExperimentalMacroEnabled(using Context) = enabled(Xmacros)

  def sourceVersionSetting(using Context): SourceVersion =
    SourceVersion.valueOf(ctx.settings.source.value)

  def sourceVersion(using Context): SourceVersion =
    if ctx.compilationUnit == null then sourceVersionSetting
    else ctx.compilationUnit.sourceVersion match
      case Some(v) => v
      case none => sourceVersionSetting

  def migrateTo3(using Context): Boolean = sourceVersion == `3.0-migration`

  /** If current source migrates to `version`, issue given warning message
   *  and return `true`, otherwise return `false`.
   */
  def warnOnMigration(msg: Message, pos: SrcPos,
      version: SourceVersion = defaultSourceVersion)(using Context): Boolean =
    if sourceVersion.isMigrating && sourceVersion.stable == version
       || version == `3.0` && migrateTo3
    then
      report.migrationWarning(msg, pos)
      true
    else
      false

end Feature