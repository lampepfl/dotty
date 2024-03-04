package dotty.tools
package dotc
package core

import Names.*
import NameOps.*
import StdNames.*
import NameTags.*
import Contexts.*
import Decorators.*

import scala.annotation.internal.sharable

/** Defines possible kinds of NameInfo of a derived name */
object NameKinds {

  // These are sharable since all NameKinds are created eagerly at the start of the program
  // before any concurrent threads are forked. for this to work, NameKinds should never
  // be created lazily or in modules that start running after compilers are forked.
  @sharable private val simpleNameKinds = util.HashMap[Int, ClassifiedNameKind]()
  @sharable private val qualifiedNameKinds = util.HashMap[Int, QualifiedNameKind]()
  @sharable private val numberedNameKinds = util.HashMap[Int, NumberedNameKind]()
  @sharable private val uniqueNameKinds = util.HashMap[String, UniqueNameKind]()

  /** A class for the info stored in a derived name */
  abstract class NameInfo {
    def kind: NameKind
    def mkString(underlying: TermName): String
    def map(f: SimpleName => SimpleName): NameInfo = this
  }

  /** An abstract base class of classes that define the kind of a derived name info */
  abstract class NameKind(val tag: Int) { self =>

    /** The info class defined by this kind */
    type ThisInfo <: Info

    /** A simple info type; some subclasses of Kind define more refined versions */
    class Info extends NameInfo { this: ThisInfo =>
      def kind: NameKind = self
      def mkString(underlying: TermName): String = self.mkString(underlying, this)
      override def toString: String = infoString
    }

    /** Does this kind define logically a new name (respectively qualified name)?
     *  Tested by the `replace` and `collect` combinators of class `Name`.
     */
    def definesNewName: Boolean = false
    def definesQualifiedName: Boolean = false

    /** Unmangle simple name `name` into a name of this kind, or return
     *  original name if this is not possible.
     */
    def unmangle(name: SimpleName): TermName = name

    /** Turn a name of this kind consisting of an `underlying` prefix
     *  and the given `info` into a string. Used to turn structured into
     *  simple name.
     */
    def mkString(underlying: TermName, info: ThisInfo): String

    /** A string used for displaying the structure of a name */
    def infoString: String
  }

  object SimpleNameKind extends NameKind(UTF8) { self =>
    type ThisInfo = Info
    val info: Info = new Info
    def mkString(underlying: TermName, info: ThisInfo): Nothing = unsupported("mkString")
    def infoString: Nothing = unsupported("infoString")
  }

  /** The kind of names that add a simple classification to an underlying name.
   */
  abstract class ClassifiedNameKind(tag: Int, val infoString: String) extends NameKind(tag) {
    type ThisInfo = Info
    val info: Info = new Info

    /** Build a new name of this kind from an underlying name */
    def apply(underlying: TermName): TermName = underlying.derived(info)

    /** Extractor operation for names of this kind */
    def unapply(name: DerivedName): Option[TermName] =  name match {
      case DerivedName(underlying, `info`) => Some(underlying)
      case _ => None
    }

    simpleNameKinds(tag) = this: @unchecked
  }

  /** The kind of names that get formed by adding a prefix to an underlying name */
  class PrefixNameKind(tag: Int, prefix: String, optInfoString: String = "")
  extends ClassifiedNameKind(tag, if (optInfoString.isEmpty) s"Prefix $prefix" else optInfoString) {
    def mkString(underlying: TermName, info: ThisInfo): String =
      underlying.qualToString(_.toString, n => prefix + n.toString)
    override def unmangle(name: SimpleName): TermName =
      if (name.startsWith(prefix)) apply(name.drop(prefix.length).asSimpleName)
      else name
  }

  /** The kind of names that get formed by appending a suffix to an underlying name */
  class SuffixNameKind(tag: Int, suffix: String, optInfoString: String = "")
  extends ClassifiedNameKind(tag, if (optInfoString.isEmpty) s"Suffix $suffix" else optInfoString) {
    def mkString(underlying: TermName, info: ThisInfo): String =
      underlying.qualToString(_.toString, n => n.toString + suffix)
    override def unmangle(name: SimpleName): TermName =
      if (name.endsWith(suffix)) apply(name.take(name.length - suffix.length).asSimpleName)
      else name
  }

  /** A base trait for infos that define an additional selector name */
  trait QualifiedInfo extends NameInfo {
    val name: SimpleName
  }

  /** The kind of qualified names, consisting of an underlying name as a prefix,
   *  followed by a separator, followed by a simple selector name.
   *
   *  A qualified names always constitutes a new name, different from its underlying name.
   */
  class QualifiedNameKind(tag: Int, val separator: String)
  extends NameKind(tag) {
    type ThisInfo = QualInfo
    case class QualInfo(name: SimpleName) extends Info with QualifiedInfo {
      override def map(f: SimpleName => SimpleName): NameInfo = new QualInfo(f(name))
      override def toString: String = s"$infoString $name"
      override def hashCode = scala.runtime.ScalaRunTime._hashCode(this) * 31 + kind.hashCode
    }

    def apply(qual: TermName, name: SimpleName): TermName =
      qual.derived(new QualInfo(name))

    /** Overloaded version used only for ExpandedName and TraitSetterName.
     *  Needed because the suffix of an expanded name may itself be expanded.
     *  For example, look at javap of scala.App.initCode
     */
    def apply(qual: TermName, name: TermName): TermName = name replace {
      case name: SimpleName => apply(qual, name)
      case AnyQualifiedName(_, _) => apply(qual, name.toSimpleName)
    }

    def unapply(name: DerivedName): Option[(TermName, SimpleName)] = name match {
      case DerivedName(qual, info: this.QualInfo) => Some((qual, info.name))
      case _ => None
    }

    override def definesNewName: Boolean = true
    override def definesQualifiedName: Boolean = true

    def mkString(underlying: TermName, info: ThisInfo): String =
      s"$underlying$separator${info.name}"

    def infoString: String = s"Qualified $separator"

    qualifiedNameKinds(tag) = this: @unchecked
  }

  /** An extractor for qualified names of an arbitrary kind */
  object AnyQualifiedName {
    def unapply(name: DerivedName): Option[(TermName, SimpleName)] = name match {
      case DerivedName(qual, info: QualifiedInfo) =>
        Some((name.underlying, info.name))
      case _ => None
    }
  }

  /** A base trait for infos that contain a number */
  trait NumberedInfo extends NameInfo {
    def num: Int
  }

  /** The kind of numbered names consisting of an underlying name and a number */
  abstract class NumberedNameKind(tag: Int, val infoString: String) extends NameKind(tag) { self =>
    type ThisInfo = NumberedInfo
    case class NumberedInfo(val num: Int) extends Info with NameKinds.NumberedInfo {
      override def toString: String = s"$infoString $num"
      override def hashCode = scala.runtime.ScalaRunTime._hashCode(this) * 31 + kind.hashCode
    }
    def apply(qual: TermName, num: Int): TermName =
      qual.derived(new NumberedInfo(num))
    def unapply(name: DerivedName): Option[(TermName, Int)] = name match {
      case DerivedName(underlying, info: this.NumberedInfo) => Some((underlying, info.num))
      case _ => None
    }
    protected def skipSeparatorAndNum(name: SimpleName, separator: String): Int =
      var i = name.length
      while i > 0 && name(i - 1).isDigit do i -= 1
      if i >= separator.length && i < name.length
          && name.slice(i - separator.length, i).toString == separator
      then i
      else -1

    numberedNameKinds(tag) = this: @unchecked
  }

  /** An extractor for numbered names of arbitrary kind */
  object AnyNumberedName {
    def unapply(name: DerivedName): Option[(TermName, Int)] = name match {
      case DerivedName(qual, info: NumberedInfo) => Some((qual, info.num))
      case _ => None
    }
  }

  /** The kind of unique names that consist of an underlying name (can be empty),
   *  a separator indicating the class of unique name, and a unique number.
   *
   *  A unique names always constitutes a new name, different from its underlying name.
   */
  case class UniqueNameKind(val separator: String)
  extends NumberedNameKind(UNIQUE, s"Unique $separator") {
    override def definesNewName: Boolean = true

    val separatorName = separator.toTermName

    def mkString(underlying: TermName, info: ThisInfo): String = {
      val safePrefix = str.sanitize(underlying.toString) + separator
      safePrefix + info.num
    }

    /** Generate fresh unique term name of this kind with given prefix name */
    def fresh(prefix: TermName = EmptyTermName)(using Context): TermName =
      ctx.compilationUnit.freshNames.newName(prefix, this)

    /** Generate fresh unique type name of this kind with given prefix name */
    def fresh(prefix: TypeName)(using Context): TypeName =
      fresh(prefix.toTermName).toTypeName

    uniqueNameKinds(separator) = this: @unchecked
  }

  /** An extractor for unique names of arbitrary kind */
  object AnyUniqueName {
    def unapply(name: DerivedName): Option[(TermName, TermName, Int)] = name match {
      case DerivedName(qual, info: NumberedInfo) =>
        info.kind match {
          case unique: UniqueNameKind => Some((qual, unique.separatorName, info.num))
          case _ => None
        }
      case _ => None
    }
  }

  /** Unique names that can be unmangled */
  class UniqueNameKindWithUnmangle(separator: String) extends UniqueNameKind(separator):
    override def unmangle(name: SimpleName): TermName =
      val i = skipSeparatorAndNum(name, separator)
      if i > 0 then
        val index = name.drop(i).toString.toInt
        val original = name.take(i - separator.length).asTermName
        apply(original, index)
      else name

  /** Names of the form `prefix . name` */
  val QualifiedName: QualifiedNameKind = new QualifiedNameKind(QUALIFIED, ".")

  /** Names of the form `prefix $ name` that are constructed as a result of flattening */
  val FlatName: QualifiedNameKind = new QualifiedNameKind(FLATTENED, "$")

  /** Names of the form `prefix $ name` that are prefixes of expanded names */
  val ExpandPrefixName: QualifiedNameKind = new QualifiedNameKind(EXPANDPREFIX, "$")

  /** Expanded names of the form `prefix $$ name`. */
  val ExpandedName: QualifiedNameKind = new QualifiedNameKind(EXPANDED, str.EXPAND_SEPARATOR) {
    private val FalseSuper = termName("$$super")
    private val FalseSuperLength = FalseSuper.length

    override def unmangle(name: SimpleName): TermName = {
      var i = name.lastIndexOfSlice(str.EXPAND_SEPARATOR)
      if (i < 0) name
      else {
        // Hack to make super accessors from traits work. They would otherwise fail because of #765
        // The problem is that in `x$$super$$plus` the expansion prefix needs to be `x`
        // instead of `x$$super`.
        if (i > FalseSuperLength && name.slice(i - FalseSuperLength, i) == FalseSuper)
          i -= FalseSuper.length

        apply(name.take(i).asTermName, name.drop(i + str.EXPAND_SEPARATOR.length).asSimpleName)
      }
    }
  }

  /** Expanded names of the form `prefix $_setter_$ name`. These only occur in Scala2. */
  val TraitSetterName: QualifiedNameKind = new QualifiedNameKind(TRAITSETTER, str.TRAIT_SETTER_SEPARATOR)

  /** Unique names of the form `prefix $ n` or `$ n $` */
  val UniqueName: UniqueNameKind = new UniqueNameKind("$") {
    override def mkString(underlying: TermName, info: ThisInfo) =
      if (underlying.isEmpty) "$" + info.num + "$" else super.mkString(underlying, info)
  }

  /** The name of the term parameter generated for a context bound:
   *
   *      def foo[T: A](...): ...
   *
   *  becomes:
   *
   *      def foo[T](...)(using evidence$1: A[T]): ...
   *
   *  The "evidence$" prefix is a convention copied from Scala 2.
   */
  val ContextBoundParamName: UniqueNameKind = new UniqueNameKindWithUnmangle("evidence$")

  /** The name of an inferred contextual function parameter:
   *
   *      val x: A ?=> B = b
   *
   *  becomes:
   *
   *      val x: A ?=> B = (contextual$1: A) ?=> b
   */
  val ContextFunctionParamName: UniqueNameKind = new UniqueNameKind("contextual$")

  /** Other unique names */
  val CanThrowEvidenceName: UniqueNameKind   = new UniqueNameKind("canThrow$")
  val TempResultName: UniqueNameKind         = new UniqueNameKind("ev$")
  val DepParamName: UniqueNameKind           = new UniqueNameKind("(param)")
  val LazyImplicitName: UniqueNameKind       = new UniqueNameKind("$_lazy_implicit_$")
  val LazyLocalName: UniqueNameKind          = new UniqueNameKind("$lzy")
  val LazyLocalInitName: UniqueNameKind      = new UniqueNameKind("$lzyINIT")
  val LazyFieldOffsetName: UniqueNameKind    = new UniqueNameKind("$OFFSET")
  val LazyBitMapName: UniqueNameKind         = new UniqueNameKind(nme.BITMAP_PREFIX.toString)
  val NonLocalReturnKeyName: UniqueNameKind  = new UniqueNameKind("nonLocalReturnKey")
  val WildcardParamName: UniqueNameKind      = new UniqueNameKind("_$")
  val TailLabelName: UniqueNameKind          = new UniqueNameKind("tailLabel")
  val TailLocalName: UniqueNameKind          = new UniqueNameKind("$tailLocal")
  val TailTempName: UniqueNameKind           = new UniqueNameKind("$tmp")
  val ExceptionBinderName: UniqueNameKind    = new UniqueNameKind("ex")
  val SkolemName: UniqueNameKind             = new UniqueNameKind("?")
  val SuperArgName: UniqueNameKind           = new UniqueNameKind("$superArg$")
  val DocArtifactName: UniqueNameKind        = new UniqueNameKind("$doc")
  val UniqueInlineName: UniqueNameKind       = new UniqueNameKind("$i")
  val InlineScrutineeName: UniqueNameKind    = new UniqueNameKind("$scrutinee")
  val InlineBinderName: UniqueNameKind       = new UniqueNameKind("$proxy")
  val MacroNames: UniqueNameKind             = new UniqueNameKind("$macro$")

  val UniqueExtMethName: UniqueNameKind = new UniqueNameKindWithUnmangle("$extension")

  /** Kinds of unique names generated by the pattern matcher */
  val PatMatStdBinderName: UniqueNameKind    = new UniqueNameKind("x")
  val PatMatAltsName: UniqueNameKind         = new UniqueNameKind("matchAlts")
  val PatMatResultName: UniqueNameKind       = new UniqueNameKind("matchResult")
  val PatMatGivenVarName: UniqueNameKind     = new UniqueNameKind("$given")

  val LocalOptInlineLocalObj: UniqueNameKind = new UniqueNameKind("ilo")

  val BoundaryName: UniqueNameKind           = new UniqueNameKind("boundary")

  /** The kind of names of default argument getters */
  val DefaultGetterName: NumberedNameKind = new NumberedNameKind(DEFAULTGETTER, "DefaultGetter") {
    def mkString(underlying: TermName, info: ThisInfo) = {
      val prefix = if (underlying.isConstructorName) nme.DEFAULT_GETTER_INIT else underlying
      prefix.toString + str.DEFAULT_GETTER + (info.num + 1)
    }
    // TODO: Reduce code duplication with UniqueExtMethName
    override def unmangle(name: SimpleName): TermName = {
      val i = skipSeparatorAndNum(name, str.DEFAULT_GETTER)
      if (i > 0) {
        val index = name.drop(i).toString.toInt - 1
        var original = name.take(i - str.DEFAULT_GETTER.length).asTermName
        if (original == nme.DEFAULT_GETTER_INIT) original = nme.CONSTRUCTOR
        apply(original, index)
      }
      else name
    }
  }

  /** Names of the form N_<outer>. Emitted by inliner, replaced by outer path
   *  in ExplicitOuter.
   */
  val OuterSelectName: NumberedNameKind = new NumberedNameKind(OUTERSELECT, "OuterSelect") {
    def mkString(underlying: TermName, info: ThisInfo) = {
      assert(underlying.isEmpty)
      s"${info.num}_<outer>"
    }
  }

  val SuperAccessorName: PrefixNameKind = new PrefixNameKind(SUPERACCESSOR, "super$")
  val InitializerName: PrefixNameKind = new PrefixNameKind(INITIALIZER, "initial$")
  val ProtectedAccessorName: PrefixNameKind = new PrefixNameKind(PROTECTEDACCESSOR, "protected$")
  val InlineAccessorName: PrefixNameKind = new PrefixNameKind(INLINEACCESSOR, "inline$")

  /** See `ConstraintHandling#LevelAvoidMap`. */
  enum AvoidNameKind(tag: Int, prefix: String) extends PrefixNameKind(tag, prefix):
    override def definesNewName = true
    case UpperBound extends AvoidNameKind(AVOIDUPPER, "(upper)")
    case LowerBound extends AvoidNameKind(AVOIDLOWER, "(lower)")
    case BothBounds extends AvoidNameKind(AVOIDBOTH, "(avoid)")

  val BodyRetainerName: SuffixNameKind = new SuffixNameKind(BODYRETAINER, "$retainedBody")
  val FieldName: SuffixNameKind = new SuffixNameKind(FIELD, "$$local") {
      override def mkString(underlying: TermName, info: ThisInfo) = underlying.toString
  }
  val ExplicitFieldName: SuffixNameKind = new SuffixNameKind(EXPLICITFIELD, "$field")
  val ExtMethName: SuffixNameKind = new SuffixNameKind(EXTMETH, "$extension")
  val ParamAccessorName: SuffixNameKind = new SuffixNameKind(PARAMACC, "$accessor")
  val ModuleClassName: SuffixNameKind = new SuffixNameKind(OBJECTCLASS, "$", optInfoString = "ModuleClass")
  val DirectMethName: SuffixNameKind = new SuffixNameKind(DIRECT, "$direct")
  val AdaptedClosureName: SuffixNameKind = new SuffixNameKind(ADAPTEDCLOSURE, "$adapted") { override def definesNewName = true }
  val SyntheticSetterName: SuffixNameKind = new SuffixNameKind(SETTER, "_$eq")

  /** A name together with a signature. Used in Tasty trees. */
  object SignedName extends NameKind(SIGNED) {

    case class SignedInfo(sig: Signature, target: TermName) extends Info {
      assert(sig ne Signature.NotAMethod)
      override def toString: String =
        val targetStr = if target.isEmpty then "" else s" @$target"
        s"$infoString $sig$targetStr"
      override def hashCode = scala.runtime.ScalaRunTime._hashCode(this) * 31 + kind.hashCode
    }
    type ThisInfo = SignedInfo

    def apply(qual: TermName, sig: Signature, target: TermName): TermName =
      qual.derived(new SignedInfo(sig, target))
    def unapply(name: DerivedName): Option[(TermName, Signature, TermName)] = name match {
      case DerivedName(underlying, info: SignedInfo) => Some((underlying, info.sig, info.target))
      case _ => None
    }

    def mkString(underlying: TermName, info: ThisInfo): String = s"$underlying[with sig ${info.sig}]"
    def infoString: String = "Signed"
  }

  /** Possible name kinds of a method that comes from Scala2 pickling info.
   *  and that need to be unmangled. Note: Scala2 protected accessors and setters
   *  can be left mangled, so they are not included in thus list.
   */
  val Scala2MethodNameKinds: List[NameKind] =
    List(DefaultGetterName, ExtMethName, UniqueExtMethName)

  def simpleNameKindOfTag      : util.ReadOnlyMap[Int, ClassifiedNameKind] = simpleNameKinds
  def qualifiedNameKindOfTag   : util.ReadOnlyMap[Int, QualifiedNameKind]  = qualifiedNameKinds
  def numberedNameKindOfTag    : util.ReadOnlyMap[Int, NumberedNameKind]   = numberedNameKinds
  def uniqueNameKindOfSeparator: util.ReadOnlyMap[String, UniqueNameKind]  = uniqueNameKinds
}
