package dotty.tools
package dotc
package config

import core.*
import Contexts.*, Symbols.*, Names.*
import StdNames.nme
import Decorators.*
import util.{SrcPos, NoSourcePosition}
import SourceVersion.*
import reporting.Message
import NameKinds.QualifiedName

object Feature:

  def experimental(str: PreName): TermName =
    QualifiedName(nme.experimental, str.toTermName)

  private def deprecated(str: PreName): TermName =
    QualifiedName(nme.deprecated, str.toTermName)

  private val namedTypeArguments = experimental("namedTypeArguments")
  private val genericNumberLiterals = experimental("genericNumberLiterals")
  val scala2macros = experimental("macros")

  val dependent = experimental("dependent")
  val erasedDefinitions = experimental("erasedDefinitions")
  val symbolLiterals = deprecated("symbolLiterals")
  val fewerBraces = experimental("fewerBraces")
  val saferExceptions = experimental("saferExceptions")
  val clauseInterleaving = experimental("clauseInterleaving")
  val pureFunctions = experimental("pureFunctions")
  val captureChecking = experimental("captureChecking")
  val into = experimental("into")
  val modularity = experimental("modularity")

  val globalOnlyImports: Set[TermName] = Set(pureFunctions, captureChecking)

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
      val info = ctx.importInfo
      info != null && info.featureImported(feature)
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

  def dependentEnabled(using Context) = enabled(dependent)

  def namedTypeArgsEnabled(using Context) = enabled(namedTypeArguments)

  def clauseInterleavingEnabled(using Context) = enabled(clauseInterleaving)

  def genericNumberLiteralsEnabled(using Context) = enabled(genericNumberLiterals)

  def scala2ExperimentalMacroEnabled(using Context) = enabled(scala2macros)

  /** Is pureFunctions enabled for this compilation unit? */
  def pureFunsEnabled(using Context) =
    enabledBySetting(pureFunctions)
    || ctx.compilationUnit.knowsPureFuns
    || ccEnabled

  /** Is captureChecking enabled for this compilation unit? */
  def ccEnabled(using Context) =
    enabledBySetting(captureChecking)
    || ctx.compilationUnit.needsCaptureChecking

  /** Is pureFunctions enabled for any of the currently compiled compilation units? */
  def pureFunsEnabledSomewhere(using Context) =
    enabledBySetting(pureFunctions)
    || ctx.run != null && ctx.run.nn.pureFunsImportEncountered
    || ccEnabledSomewhere

  /** Is captureChecking enabled for any of the currently compiled compilation units? */
  def ccEnabledSomewhere(using Context) =
    if ctx.run != null then ctx.run.nn.ccEnabledSomewhere
    else enabledBySetting(captureChecking)

  def sourceVersionSetting(using Context): SourceVersion =
    SourceVersion.valueOf(ctx.settings.source.value)

  def sourceVersion(using Context): SourceVersion =
    ctx.compilationUnit.sourceVersion match
      case Some(v) => v
      case none => sourceVersionSetting

  def migrateTo3(using Context): Boolean =
    sourceVersion == `3.0-migration`

  def fewerBracesEnabled(using Context) =
    sourceVersion.isAtLeast(`3.3`) || enabled(fewerBraces)

  /** If current source migrates to `version`, issue given warning message
   *  and return `true`, otherwise return `false`.
   */
  def warnOnMigration(msg: Message, pos: SrcPos, version: SourceVersion)(using Context): Boolean =
    if sourceVersion.isMigrating && sourceVersion.stable == version
       || (version == `3.0` || version == `3.1`) && migrateTo3
    then
      report.migrationWarning(msg, pos)
      true
    else
      false

  def checkExperimentalFeature(which: String, srcPos: SrcPos, note: => String = "")(using Context) =
    if !isExperimentalEnabled then
      report.error(
        em"""Experimental $which may only be used under experimental mode:
            |  1. In a definition marked as @experimental
            |  2. Compiling with the -experimental compiler flag
            |  3. With a nightly or snapshot version of the compiler$note
          """, srcPos)

  private def ccException(sym: Symbol)(using Context): Boolean =
    ccEnabled && defn.ccExperimental.contains(sym)

  def checkExperimentalDef(sym: Symbol, srcPos: SrcPos)(using Context) =
    if !isExperimentalEnabled then
      val experimentalSym =
        if sym.hasAnnotation(defn.ExperimentalAnnot) then sym
        else if sym.owner.hasAnnotation(defn.ExperimentalAnnot) then sym.owner
        else NoSymbol
      if !ccException(experimentalSym) then
        val symMsg =
          if experimentalSym.exists
          then i"$experimentalSym is marked @experimental"
          else i"$sym inherits @experimental"
        report.error(em"$symMsg and therefore may only be used in an experimental scope.", srcPos)

  /** Check that experimental compiler options are only set for snapshot or nightly compiler versions. */
  def checkExperimentalSettings(using Context): Unit =
    for setting <- ctx.settings.language.value
        if setting.startsWith("experimental.") && setting != "experimental.macros"
    do checkExperimentalFeature(s"feature $setting", NoSourcePosition)

  def isExperimentalEnabled(using Context): Boolean =
    (Properties.experimental || ctx.settings.experimental.value) && !ctx.settings.YnoExperimental.value

  /** Handle language import `import language.<prefix>.<imported>` if it is one
   *  of the global imports `pureFunctions` or `captureChecking`. In this case
   *  make the compilation unit's and current run's fields accordingly.
   *  @return true iff import that was handled
   */
  def handleGlobalLanguageImport(prefix: TermName, imported: Name)(using Context): Boolean =
    val fullFeatureName = QualifiedName(prefix, imported.asTermName)
    if fullFeatureName == pureFunctions then
      ctx.compilationUnit.knowsPureFuns = true
      if ctx.run != null then ctx.run.nn.pureFunsImportEncountered = true
      true
    else if fullFeatureName == captureChecking then
      ctx.compilationUnit.needsCaptureChecking = true
      if ctx.run != null then ctx.run.nn.ccEnabledSomewhere = true
      true
    else
      false
end Feature
