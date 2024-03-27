package dotty.tools.dotc.core

/** A collection of mode bits that are part of a context.
 *
 * What's the difference between a boolean setting and a Mode?
 * A setting is usually valid for the entire compilation run, whereas a mode is context specific.
 * Changing a setting in a context creates a new SettingsState in that context, which is a relatively big object.
 * By comparison, a mode is just an Int.
 * But, Mode bits are a scarce resource, so for low priority situations, just reset the state with a setting.
 * Also, a setting is externally settable, while a mode isn't.
 */
case class Mode(val bits: Int) extends AnyVal {
  import Mode.*
  def | (that: Mode): Mode = Mode(bits | that.bits)
  def & (that: Mode): Mode = Mode(bits & that.bits)
  def &~ (that: Mode): Mode = Mode(bits & ~that.bits)
  def is (that: Mode): Boolean = (bits & that.bits) == that.bits

  def isExpr: Boolean = (this & PatternOrTypeBits) == None

  /** Are we in the body of quoted pattern? */
  def isQuotedPattern: Boolean = (this & QuotedPatternBits) != None

  override def toString: String =
    (0 until 31).filter(i => (bits & (1 << i)) != 0).map(modeName).mkString("Mode(", ",", ")")

  def ==(that: Mode): Boolean = this.bits == that.bits
  def !=(that: Mode): Boolean = this.bits != that.bits
}

object Mode {
  val None: Mode = Mode(0)

  private val modeName = new Array[String](32)

  def newMode(bit: Int, name: String): Mode = {
    modeName(bit) = name
    Mode(1 << bit)
  }

  val Pattern: Mode = newMode(0, "Pattern")
  val Type: Mode = newMode(1, "Type")

  val PatternOrTypeBits: Mode = Pattern | Type

  val ImplicitsEnabled: Mode = newMode(2, "ImplicitsEnabled")
  val InferringReturnType: Mode = newMode(3, "InferringReturnType")

  /** This mode bit is set if we collect information without reference to a valid
   *  context with typerstate and constraint. This is typically done when we
   *  cache the eligibility of implicits. Caching needs to be done across different constraints.
   *  Therefore, if TypevarsMissContext is set, subtyping becomes looser, and assumes
   *  that TypeParamRefs can be sub- and supertypes of anything. See TypeComparer.
   */
  val TypevarsMissContext: Mode = newMode(4, "TypevarsMissContext")

  /** Are we looking for cyclic references? */
  val CheckCyclic: Mode = newMode(5, "CheckCyclic")

  /** We are in arguments of HOAS pattern in quote pattern matching
   *  e.g. x, y, z in a quote pattern '{ ... $a(x, y, z) ... }
   *
   *  This mode keep typer from inserting contextual parameters to a contextual method without arguments.
   *  (See tests/run-macros/i17905 for motivating examples)
   */
  val InQuotePatternHoasArgs: Mode = newMode(6, "InQuotePatternHoasArgs")

  /** We are in a pattern alternative */
  val InPatternAlternative: Mode = newMode(7, "InPatternAlternative")

  /** Make subtyping checks instead infer constraints necessarily following from given subtyping relation.
   *
   *  This enables changing [[GadtConstraint]] and alters how [[TypeComparer]] approximates constraints.
   */
  val GadtConstraintInference: Mode = newMode(8, "GadtConstraintInference")

  /** Assume -language:strictEquality */
  val StrictEquality: Mode = newMode(9, "StrictEquality")

  /** We are currently printing something: avoid producing more logs about
   *  the printing.
   */
  val Printing: Mode = newMode(10, "Printing")

  /** Are we in a quote the body of quoted type pattern? */
  val QuotedTypePattern: Mode = newMode(11, "QuotedTypePattern")

  /** We are currently in a `viewExists` check. In that case, ambiguous
   *  implicits checks are disabled and we succeed with the first implicit
   *  found.
   */
  val ImplicitExploration: Mode = newMode(12, "ImplicitExploration")

  /** We are currently unpickling Scala2 info */
  val Scala2Unpickling: Mode = newMode(13, "Scala2Unpickling")

  /** Signifies one of two possible situations:
   *   1. We are currently checking bounds to be non-empty, so we should not
   *      do any widening when computing members of refined types.
   *   2. We are currently checking self type conformance, so we should not
   *      ignore capture sets added to otherwise pure classes (only needed
   *      for capture checking).
   */
  val CheckBoundsOrSelfType: Mode = newMode(14, "CheckBoundsOrSelfType")

  /** Use previous Scheme for implicit resolution. Currently significant
   *  in 3.0-migration where we use Scala-2's scheme instead and in 3.5-migration
   *  where we use the previous scheme up to 3.4 instead.
   */
  val OldImplicitResolution: Mode = newMode(15, "OldImplicitResolution")

  /** Treat CapturingTypes as plain AnnotatedTypes even in phase CheckCaptures.
   *  Reuses the value of OldImplicitResolution to save Mode bits.
   *  This is OK since OldImplicitResolution only affects implicit search, which
   *  is done during phases Typer and Inlinig, and IgnoreCaptures only has an
   *  effect during phase CheckCaptures.
   */
  val IgnoreCaptures = OldImplicitResolution

  /** Allow hk applications of type lambdas to wildcard arguments;
   *  used for checking that such applications do not normally arise
   */
  val AllowLambdaWildcardApply: Mode = newMode(16, "AllowHKApplyToWildcards")

  /** Read original positions when unpickling from TASTY */
  val ReadPositions: Mode = newMode(17, "ReadPositions")

  /** We are elaborating the fully qualified name of a package clause.
   *  In this case, identifiers should never be imported.
   */
  val InPackageClauseName: Mode = newMode(19, "InPackageClauseName")

  /** We are in the IDE */
  val Interactive: Mode = newMode(20, "Interactive")

  /** We are typing the body of an inline method */
  val InlineableBody: Mode = newMode(21, "InlineableBody")

  /** We are synthesizing the receiver of an extension method */
  val SynthesizeExtMethodReceiver: Mode = newMode(23, "SynthesizeExtMethodReceiver")

  /** Are we trying to find a hidden implicit? */
  val FindHiddenImplicits: Mode = newMode(24, "FindHiddenImplicits")

  /** Are we in a quote the body of quoted expression pattern? */
  val QuotedExprPattern: Mode = newMode(25, "QuotedExprPattern")

  val QuotedPatternBits: Mode = QuotedExprPattern | QuotedTypePattern

  /** Are we typechecking the rhs of an extension method? */
  val InExtensionMethod: Mode = newMode(26, "InExtensionMethod")

  /** Are we resolving a TypeTest node? */
  val InTypeTest: Mode = newMode(27, "InTypeTest")

  /** Are we enforcing null safety? */
  val SafeNulls = newMode(28, "SafeNulls")

  /** We are typing the body of the condition of an `inline if` or the scrutinee of an `inline match`
   *  This mode forces expansion of inline calls in those positions even during typing.
   */
  val ForceInline: Mode = newMode(29, "ForceInline")

  /** This mode is enabled when we check Java overriding in explicit nulls.
   *  Type `Null` becomes a subtype of non-primitive value types in TypeComparer.
   */
  val RelaxedOverriding: Mode = newMode(30, "RelaxedOverriding")

  /** Skip inlining of methods. */
  val NoInline: Mode = newMode(31, "NoInline")
}
