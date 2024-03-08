package dotty.tools.dotc
package core

import scala.collection.mutable
import scala.annotation.switch
import scala.annotation.internal.sharable
import Names.*
import Symbols.*
import Contexts.*

object StdNames {

/** Base strings from which synthetic names are derived. */

  object str {
    inline val SETTER_SUFFIX            = "_="
    inline val EXPAND_SEPARATOR         = "$$"
    inline val TRAIT_SETTER_SEPARATOR   = "$_setter_$"
    inline val SUPER_PREFIX             = "super$"
    inline val INITIALIZER_PREFIX       = "initial$"
    inline val AVOID_CLASH_SUFFIX       = "$_avoid_name_clash_$"
    inline val MODULE_SUFFIX            = "$"
    inline val TOPLEVEL_SUFFIX          = "$package"
    inline val NAME_JOIN                = "$"
    inline val DEFAULT_GETTER           = "$default$"
    inline val LOCALDUMMY_PREFIX        = "<local "       // owner of local blocks
    inline val ANON_CLASS               = "$anon"
    inline val ANON_FUN                 = "$anonfun"

    inline val REPL_SESSION_LINE  = "rs$line$"
    inline val REPL_ASSIGN_SUFFIX = "$assign"
    inline val REPL_RES_PREFIX    = "res"

    inline val MODULE_INSTANCE_FIELD      = "MODULE$"

    inline val Function                   = "Function"
    inline val ContextFunction            = "ContextFunction"
    inline val ErasedContextFunction      = "ErasedContextFunction"
    inline val AbstractFunction           = "AbstractFunction"
    inline val Tuple                      = "Tuple"
    inline val Product                    = "Product"

    @sharable
    private val disallowed = java.util.regex.Pattern.compile("""[<>]""").nn
    def sanitize(str: String): String = disallowed.matcher(str).nn.replaceAll("""\$""").nn
  }

  abstract class DefinedNames[N <: Name] {
    protected implicit def fromString(s: String): N
    protected def fromName(name: Name): N = fromString(name.toString)

    private val kws = mutable.Set[N]()
    protected def kw(name: N): N = { kws += name; name }

    final val keywords: collection.Set[N] = kws
  }

  abstract class ScalaNames[N <: Name] extends DefinedNames[N] {
    protected def encode(s: String): N = fromName(fromString(s).encode)

// Keywords, need to come first -----------------------

    final val ABSTRACTkw: N  = kw("abstract")
    final val CASEkw: N      = kw("case")
    final val CLASSkw: N     = kw("class")
    final val CATCHkw: N     = kw("catch")
    final val DEFkw: N       = kw("def")
    final val DOkw: N        = kw("do")
    final val ELSEkw: N      = kw("else")
    final val EXTENDSkw: N   = kw("extends")
    final val FALSEkw: N     = kw("false")
    final val FINALkw: N     = kw("final")
    final val FINALLYkw: N   = kw("finally")
    final val FORkw: N       = kw("for")
    final val FORSOMEkw: N   = kw("forSome")
    final val IFkw: N        = kw("if")
    final val IMPLICITkw: N  = kw("implicit")
    final val IMPORTkw: N    = kw("import")
    final val LAZYkw: N      = kw("lazy")
    final val MACROkw: N     = kw("macro")
    final val MATCHkw: N     = kw("match")
    final val NEWkw: N       = kw("new")
    final val NULLkw: N      = kw("null")
    final val OBJECTkw: N    = kw("object")
    final val OVERRIDEkw: N  = kw("override")
    final val PACKAGEkw: N   = kw("package")
    final val PRIVATEkw: N   = kw("private")
    final val PROTECTEDkw: N = kw("protected")
    final val RETURNkw: N    = kw("return")
    final val SEALEDkw: N    = kw("sealed")
    final val SUPERkw: N     = kw("super")
    final val THENkw: N      = kw("then")
    final val THISkw: N      = kw("this")
    final val THROWkw: N     = kw("throw")
    final val TRAITkw: N     = kw("trait")
    final val TRUEkw: N      = kw("true")
    final val TRYkw: N       = kw("try")
    final val TYPEkw: N      = kw("type")
    final val VALkw: N       = kw("val")
    final val VARkw: N       = kw("var")
    final val WITHkw: N      = kw("with")
    final val WHILEkw: N     = kw("while")
    final val YIELDkw: N     = kw("yield")
    final val DOTkw: N       = kw(".")
    final val USCOREkw: N    = kw("_")
    final val COLONkw: N     = kw(":")
    final val EQUALSkw: N    = kw("=")
    final val ARROWkw: N     = kw("=>")
    final val LARROWkw: N    = kw("<-")
    final val SUBTYPEkw: N   = kw("<:")
    final val VIEWBOUNDkw: N = kw("<%")
    final val SUPERTYPEkw: N = kw(">:")
    final val HASHkw: N      = kw("#")
    final val ATkw: N        = kw("@")

    val ANON_CLASS: N                 = str.ANON_CLASS
    val ANON_FUN: N                   = str.ANON_FUN
    val BITMAP_PREFIX: N              = "bitmap$"  // @darkdimius: $bitmap? Also, the next 4 names are unused.
    val BITMAP_NORMAL: N              = BITMAP_PREFIX         // initialization bitmap for public/protected lazy vals
    val BITMAP_TRANSIENT: N           = s"${BITMAP_PREFIX}trans$$"    // initialization bitmap for transient lazy vals
    val BITMAP_CHECKINIT: N           = s"${BITMAP_PREFIX}init$$"      // initialization bitmap for checkinit values
    val BITMAP_CHECKINIT_TRANSIENT: N = s"${BITMAP_PREFIX}inittrans$$" // initialization bitmap for transient checkinit values
    val CC_REACH: N                   = "$reach"
    val DEFAULT_GETTER: N             = str.DEFAULT_GETTER
    val DEFAULT_GETTER_INIT: N        = "$lessinit$greater"
    val DO_WHILE_PREFIX: N            = "doWhile$"
    val DOLLAR_VALUES: N              = "$values"
    val DOLLAR_NEW: N                 = "$new"
    val EMPTY: N                      = ""
    val EMPTY_PACKAGE: N              = "<empty>"
    val EXCEPTION_RESULT_PREFIX: N    = "exceptionResult"
    val EXPAND_SEPARATOR: N           = str.EXPAND_SEPARATOR
    val IMPORT: N                     = "<import>"
    val INTO: N                       = "$into"
    val MODULE_SUFFIX: N              = str.MODULE_SUFFIX
    val OPS_PACKAGE: N                = "<special-ops>"
    val OVERLOADED: N                 = "<overloaded>"
    val PACKAGE: N                    = "package"
    val ROOT: N                       = "<root>"
    val SPECIALIZED_SUFFIX: N         = "$sp"
    val SUPER_PREFIX: N               = "super$"
    val WHILE_PREFIX: N               = "while$"
    val DEFAULT_EXCEPTION_NAME: N     = "ex$"
    val INITIALIZER_PREFIX: N         = "initial$"
    val BOUNDTYPE_ANNOT: N            = "$boundType$"
    val QUOTE: N                      = "'"
    val TYPE_QUOTE: N                 = "type_'"
    val TRAIT_SETTER_SEPARATOR: N     = str.TRAIT_SETTER_SEPARATOR
    val AMBIGUOUS: N                  = "/* ambiguous */"
    val MISSING: N                    = "/* missing */"

    // value types (and AnyRef) are all used as terms as well
    // as (at least) arguments to the @specialize annotation.
    final val Boolean: N = "Boolean"
    final val Byte: N    = "Byte"
    final val Char: N    = "Char"
    final val Double: N  = "Double"
    final val Float: N   = "Float"
    final val Int: N     = "Int"
    final val Long: N    = "Long"
    final val Short: N   = "Short"
    final val Unit: N    = "Unit"

    final val ScalaValueNames: _root_.scala.List[N] =
      _root_.scala.List(Byte, Char, Short, Int, Long, Float, Double, Boolean, Unit)

    // some types whose companions we utilize
    final val AnyRef: N     = "AnyRef"
    final val Array: N      = "Array"
    final val List: N       = "List"
    final val Seq: N        = "Seq"
    final val Symbol: N     = "Symbol"
    final val ClassTag: N   = "ClassTag"
    final val classTag: N   = "classTag"
    final val WeakTypeTag: N = "WeakTypeTag"
    final val TypeTag : N   = "TypeTag"
    final val typeTag: N    = "typeTag"
    final val Expr: N       = "Expr"
    final val String: N     = "String"
    final val Option: N     = "Option"
    final val Annotation: N = "Annotation"

    // fictions we use as both types and terms
    final val ERROR: N    = "<error>"
    final val NO_NAME: N  = "<none>"  // formerly NOSYMBOL
    final val WILDCARD: N = "_"

// ----- Type names -----------------------------------------

    final val BYNAME_PARAM_CLASS: N             = "<byname>"
    final val EQUALS_PATTERN: N                 = "<equals>"
    final val LOCAL_CHILD: N                    = "<local child>"
    final val REPEATED_PARAM_CLASS: N           = "<repeated>"
    final val WILDCARD_STAR: N                  = "_*"
    final val REIFY_TREECREATOR_PREFIX: N       = "$treecreator"
    final val REIFY_TYPECREATOR_PREFIX: N       = "$typecreator"

    final val Any: N                 = "Any"
    final val AnyKind: N             = "AnyKind"
    final val AnyVal: N              = "AnyVal"
    final val ExprApi: N             = "ExprApi"
    final val Mirror: N              = "Mirror"
    final val Nothing: N             = "Nothing"
    final val NotNull: N             = "NotNull"
    final val Null: N                = "Null"
    final val Object: N              = "Object"
    final val FromJavaObject: N      = "<FromJavaObject>"
    final val Record: N              = "Record"
    final val Product: N             = "Product"
    final val PartialFunction: N     = "PartialFunction"
    final val PrefixType: N          = "PrefixType"
    final val Serializable: N        = "Serializable"
    final val Singleton: N           = "Singleton"
    final val Throwable: N           = "Throwable"
    final val IOOBException: N       = "IndexOutOfBoundsException"
    final val FunctionXXL: N         = "FunctionXXL"

    final val Abs: N                  = "Abs"
    final val And: N                  = "&&"
    final val BitwiseAnd: N           = "BitwiseAnd"
    final val BitwiseOr: N            = "BitwiseOr"
    final val Div: N                  = "/"
    final val Equals: N               = "=="
    final val Ge: N                   = ">="
    final val Gt: N                   = ">"
    final val IsConst: N              = "IsConst"
    final val Le: N                   = "<="
    final val Length: N               = "Length"
    final val Lt: N                   = "<"
    final val Matches: N              = "Matches"
    final val Max: N                  = "Max"
    final val Min: N                  = "Min"
    final val Minus: N                = "-"
    final val Mod: N                  = "%"
    final val Negate: N               = "Negate"
    final val Not: N                  = "!"
    final val NotEquals: N            = "!="
    final val NumberOfLeadingZeros: N = "NumberOfLeadingZeros"
    final val Or: N                   = "||"
    final val Plus: N                 = "+"
    final val S: N                    = "S"
    final val Substring: N            = "Substring"
    final val CharAt: N               = "CharAt"
    final val Times: N                = "*"
    final val ToInt: N                = "ToInt"
    final val ToLong: N               = "ToLong"
    final val ToFloat: N              = "ToFloat"
    final val ToDouble: N             = "ToDouble"
    final val ToString: N             = "ToString"
    final val Xor: N                  = "^"

    final val ClassManifest: N       = "ClassManifest"
    final val Enum: N                = "Enum"
    final val Group: N               = "Group"
    final val Tree: N                = "Tree"
    final val Type : N               = "Type"
    final val TypeTree: N            = "TypeTree"
    final val Underlying: N          = "Underlying"

    // Annotation simple names, used in Namer
    final val BeanPropertyAnnot: N = "BeanProperty"
    final val BooleanBeanPropertyAnnot: N = "BooleanBeanProperty"
    final val bridgeAnnot: N = "bridge"

    // Classfile Attributes
    final val AnnotationDefaultATTR: N            = "AnnotationDefault"
    final val BridgeATTR: N                       = "Bridge"
    final val CodeATTR: N                         = "Code"
    final val ConstantValueATTR: N                = "ConstantValue"
    final val DeprecatedATTR: N                   = "Deprecated"
    final val ExceptionsATTR: N                   = "Exceptions"
    final val InnerClassesATTR: N                 = "InnerClasses"
    final val MethodParametersATTR: N             = "MethodParameters"
    final val LineNumberTableATTR: N              = "LineNumberTable"
    final val LocalVariableTableATTR: N           = "LocalVariableTable"
    final val RuntimeVisibleAnnotationATTR: N     = "RuntimeVisibleAnnotations"   // RetentionPolicy.RUNTIME
    final val RuntimeInvisibleAnnotationATTR: N   = "RuntimeInvisibleAnnotations" // RetentionPolicy.CLASS
    final val RuntimeParamAnnotationATTR: N       = "RuntimeVisibleParameterAnnotations" // RetentionPolicy.RUNTIME (annotations on parameters)
    final val ScalaATTR: N                        = "Scala"
    final val ScalaSignatureATTR: N               = "ScalaSig"
    final val TASTYATTR: N                        = "TASTY"
    final val SignatureATTR: N                    = "Signature"
    final val SourceFileATTR: N                   = "SourceFile"
    final val SyntheticATTR: N                    = "Synthetic"


// ----- Term names -----------------------------------------

    // Compiler-internal
    val CAPTURE_ROOT: N             = "cap"
    val CBCompanion: N              = "<context-bound-companion>"
    val CONSTRUCTOR: N              = "<init>"
    val STATIC_CONSTRUCTOR: N       = "<clinit>"
    val EVT2U: N                    = "evt2u$"
    val EQEQ_LOCAL_VAR: N           = "eqEqTemp$"
    val LAZY_FIELD_OFFSET: N        = "OFFSET$"
    val OUTER: N                    = "$outer"
    val REFINE_CLASS: N             = "<refinement>"
    val ROOTPKG: N                  = "_root_"
    val SELF: N                     = "$this"
    val SKOLEM: N                   = "<skolem>"
    val TRAIT_CONSTRUCTOR: N        = "$init$"
    val TRY_BLOCK: N                = "<try block>"
    val THROWS: N                   = "$throws"
    val U2EVT: N                    = "u2evt$"
    val ALLARGS: N                  = "$allArgs"
    val UPARROW: N                  = "^"

    final val Nil: N                = "Nil"
    final val Predef: N             = "Predef"
    final val BoxedUnit: N          = "BoxedUnit"

    val x_0 : N  = "x$0"
    val x_1 : N  = "x$1"
    val x_2 : N  = "x$2"
    val x_3 : N  = "x$3"
    val x_4 : N  = "x$4"
    val x_5 : N  = "x$5"
    val x_6 : N  = "x$6"
    val x_7 : N  = "x$7"
    val x_8 : N  = "x$8"
    val x_9 : N  = "x$9"
    val _1 : N  = "_1"
    val _2 : N  = "_2"
    val _3 : N  = "_3"
    val _4 : N  = "_4"
    val _5 : N  = "_5"
    val _6 : N  = "_6"
    val _7 : N  = "_7"
    val _8 : N  = "_8"
    val _9 : N  = "_9"
    val _10 : N = "_10"
    val _11 : N = "_11"
    val _12 : N = "_12"
    val _13 : N = "_13"
    val _14 : N = "_14"
    val _15 : N = "_15"
    val _16 : N = "_16"
    val _17 : N = "_17"
    val _18 : N = "_18"
    val _19 : N = "_19"
    val _20 : N = "_20"
    val _21 : N = "_21"
    val _22 : N = "_22"

    val * : N   = "*"
    val ? : N   = "?"
    val ??? : N = "???"

    val genericWrapArray: N     = "genericWrapArray"
    def wrapRefArray: N         = "wrapRefArray"
    def wrapXArray(clsName: Name): N = "wrap" + clsName + "Array"

    // Compiler utilized names

    val AnnotatedType: N        = "AnnotatedType"
    val AppliedTypeTree: N      = "AppliedTypeTree"
    val ArrayAnnotArg: N        = "ArrayAnnotArg"
    val CAP: N                  = "CAP"
    val Constant: N             = "Constant"
    val ConstantType: N         = "ConstantType"
    val Eql: N                  = "Eql"
    val EnumValue: N            = "EnumValue"
    val ExistentialTypeTree: N  = "ExistentialTypeTree"
    val Flag : N                = "Flag"
    val Ident: N                = "Ident"
    val Import: N               = "Import"
    val Literal: N              = "Literal"
    val LiteralAnnotArg: N      = "LiteralAnnotArg"
    val Matchable: N            = "Matchable"
    val MatchCase: N            = "MatchCase"
    val MirroredElemTypes: N    = "MirroredElemTypes"
    val MirroredElemLabels: N   = "MirroredElemLabels"
    val MirroredLabel: N        = "MirroredLabel"
    val MirroredMonoType: N     = "MirroredMonoType"
    val MirroredType: N         = "MirroredType"
    val Modifiers: N            = "Modifiers"
    val NestedAnnotArg: N       = "NestedAnnotArg"
    val NoFlags: N              = "NoFlags"
    val NoPrefix: N             = "NoPrefix"
    val NoSymbol: N             = "NoSymbol"
    val NoType: N               = "NoType"
    val Pair: N                 = "Pair"
    val Ref: N                  = "Ref"
    val RootPackage: N          = "RootPackage"
    val RootClass: N            = "RootClass"
    val Select: N               = "Select"
    val Self: N                 = "Self"
    val Shape: N                = "Shape"
    val StringContext: N        = "StringContext"
    val This: N                 = "This"
    val ThisType: N             = "ThisType"
    val Tuple2: N               = "Tuple2"
    val TYPE_ : N               = "TYPE"
    val TypeApply: N            = "TypeApply"
    val TypeRef: N              = "TypeRef"
    val UNIT : N                = "UNIT"
    val WitnessNames: N         = "WitnessNames"
    val acc: N                  = "acc"
    val adhocExtensions: N      = "adhocExtensions"
    val andThen: N              = "andThen"
    val annotation: N           = "annotation"
    val any2stringadd: N        = "any2stringadd"
    val anyHash: N              = "anyHash"
    val anyValClass: N          = "anyValClass"
    val append: N               = "append"
    val apply: N                = "apply"
    val applyDynamic: N         = "applyDynamic"
    val applyDynamicNamed: N    = "applyDynamicNamed"
    val applyOrElse: N          = "applyOrElse"
    val args : N                = "args"
    val argv : N                = "argv"
    val argGetter : N           = "argGetter"
    val arrayClass: N           = "arrayClass"
    val arrayElementClass: N    = "arrayElementClass"
    val arrayType: N            = "arrayType"
    val arrayValue: N           = "arrayValue"
    val array_apply : N         = "array_apply"
    val array_clone : N         = "array_clone"
    val array_length : N        = "array_length"
    val array_update : N        = "array_update"
    val arraycopy: N            = "arraycopy"
    val as: N                   = "as"
    val asTerm: N               = "asTerm"
    val asModule: N             = "asModule"
    val asMethod: N             = "asMethod"
    val asType: N               = "asType"
    val asClass: N              = "asClass"
    val asInstanceOf_ : N       = "asInstanceOf"
    val asInstanceOfPM: N       = "$asInstanceOf$"
    val assert_ : N             = "assert"
    val assume_ : N             = "assume"
    val box: N                  = "box"
    val break: N                = "break"
    val build : N               = "build"
    val bundle: N               = "bundle"
    val bytes: N                = "bytes"
    val canEqual_ : N           = "canEqual"
    val canEqualAny : N         = "canEqualAny"
    val capIn: N                = "capIn"
    val caps: N                 = "caps"
    val captureChecking: N      = "captureChecking"
    val checkInitialized: N     = "checkInitialized"
    val classOf: N              = "classOf"
    val classType: N            = "classType"
    val clone_ : N              = "clone"
    val cmd: N                  = "cmd"
    val command: N              = "command"
    val common: N               = "common"
    val compiletime : N         = "compiletime"
    val compose: N              = "compose"
    val conforms_ : N           = "$conforms"
    val contents: N             = "contents"
    val copy: N                 = "copy"
    val create: N               = "create"
    val currentMirror: N        = "currentMirror"
    val curried: N              = "curried"
    val deferred: N             = "deferred"
    val definitions: N          = "definitions"
    val delayedInit: N          = "delayedInit"
    val delayedInitArg: N       = "delayedInit$body"
    val deprecated: N           = "deprecated"
    val derived: N              = "derived"
    val derives: N              = "derives"
    val doubleHash: N           = "doubleHash"
    val dotty: N                = "dotty"
    val drop: N                 = "drop"
    val dynamics: N             = "dynamics"
    val elem: N                 = "elem"
    val elems: N                = "elems"
    val emptyValDef: N          = "emptyValDef"
    val end: N                  = "end"
    val ensureAccessible : N    = "ensureAccessible"
    val eq: N                   = "eq"
    val eqInstance: N           = "eqInstance"
    val equalsNumChar : N       = "equalsNumChar"
    val equalsNumNum : N        = "equalsNumNum"
    val equalsNumObject : N     = "equalsNumObject"
    val equals_ : N             = "equals"
    val erased: N               = "erased"
    val error: N                = "error"
    val eval: N                 = "eval"
    val eqlAny: N               = "eqlAny"
    val ex: N                   = "ex"
    val extension: N            = "extension"
    val experimental: N         = "experimental"
    val f: N                    = "f"
    val false_ : N              = "false"
    val filter: N               = "filter"
    val finalize_ : N           = "finalize"
    val find_ : N               = "find"
    val flagsFromBits : N       = "flagsFromBits"
    val flatMap: N              = "flatMap"
    val floatHash: N            = "floatHash"
    val foreach: N              = "foreach"
    val format: N               = "format"
    val fromDigits: N           = "fromDigits"
    val fromProduct: N          = "fromProduct"
    val genericArrayOps: N      = "genericArrayOps"
    val genericClass: N         = "genericClass"
    val get: N                  = "get"
    val getClass_ : N           = "getClass"
    val getOrElse: N            = "getOrElse"
    val hasNext: N              = "hasNext"
    val hashCode_ : N           = "hashCode"
    val _hashCode_ : N          = "_hashCode"
    val hash_ : N               = "hash"
    val head: N                 = "head"
    val higherKinds: N          = "higherKinds"
    val idx: N                  = "idx"
    val identity: N             = "identity"
    val implicitConversions: N  = "implicitConversions"
    val implicitly: N           = "implicitly"
    val in: N                   = "in"
    val inline: N               = "inline"
    val infix: N                = "infix"
    val info: N                 = "info"
    val inlinedEquals: N        = "inlinedEquals"
    val internal: N             = "internal"
    val into: N                 = "into"
    val isArray: N              = "isArray"
    val isDefinedAt: N          = "isDefinedAt"
    val isDefinedAtImpl: N      = "$isDefinedAt"
    val isDefined: N            = "isDefined"
    val isEmpty: N              = "isEmpty"
    val isInstanceOf_ : N       = "isInstanceOf"
    val isInstanceOfPM: N       = "$isInstanceOf$"
    val java: N                 = "java"
    val key: N                  = "key"
    val label: N                = "label"
    val lang: N                 = "lang"
    val language: N             = "language"
    val length: N               = "length"
    val lengthCompare: N        = "lengthCompare"
    val local: N                = "local"
    val longHash: N             = "longHash"
    val macroThis : N           = "_this"
    val macroContext : N        = "c"
    val main: N                 = "main"
    val manifest: N             = "manifest"
    val ManifestFactory: N      = "ManifestFactory"
    val manifestToTypeTag: N    = "manifestToTypeTag"
    val map: N                  = "map"
    val maybeCapability: N      = "maybeCapability"
    val materializeClassTag: N  = "materializeClassTag"
    val materializeWeakTypeTag: N = "materializeWeakTypeTag"
    val materializeTypeTag: N   = "materializeTypeTag"
    val mirror : N              = "mirror"
    val moduleClass : N         = "moduleClass"
    val name: N                 = "name"
    val nameDollar: N           = "$name"
    val ne: N                   = "ne"
    val newFreeTerm: N          = "newFreeTerm"
    val newFreeType: N          = "newFreeType"
    val newScopeWith: N         = "newScopeWith"
    val next: N                 = "next"
    val nmeNewTermName: N       = "newTermName"
    val nmeNewTypeName: N       = "newTypeName"
    val noAutoTupling: N        = "noAutoTupling"
    val normalize: N            = "normalize"
    val notifyAll_ : N          = "notifyAll"
    val notify_ : N             = "notify"
    val null_ : N               = "null"
    val ofDim: N                = "ofDim"
    val on: N                   = "on"
    val opaque: N               = "opaque"
    val open: N                 = "open"
    val ordinal: N              = "ordinal"
    val ordinalDollar: N        = "$ordinal"
    val ordinalDollar_ : N      = "_$ordinal"
    val origin: N               = "origin"
    val parameters: N           = "parameters"
    val parts: N                = "parts"
    val postfixOps: N           = "postfixOps"
    val prefix : N              = "prefix"
    val processEscapes: N       = "processEscapes"
    val productArity: N         = "productArity"
    val productElement: N       = "productElement"
    val productElementName: N   = "productElementName"
    val productIterator: N      = "productIterator"
    val productPrefix: N        = "productPrefix"
    val quotes : N              = "quotes"
    val raw_ : N                = "raw"
    val refl: N                 = "refl"
    val reflect: N              = "reflect"
    val reflectiveSelectable: N = "reflectiveSelectable"
    val reify : N               = "reify"
    val releaseFence : N        = "releaseFence"
    val retains: N              = "retains"
    val retainsByName: N        = "retainsByName"
    val retainsCap: N           = "retainsCap"
    val rootMirror : N          = "rootMirror"
    val run: N                  = "run"
    val runOrElse: N            = "runOrElse"
    val runtime: N              = "runtime"
    val runtimeClass: N         = "runtimeClass"
    val runtimeMirror: N        = "runtimeMirror"
    val s: N                    = "s"
    val sameElements: N         = "sameElements"
    val scala : N               = "scala"
    val selectDynamic: N        = "selectDynamic"
    val selectOverloadedMethod: N = "selectOverloadedMethod"
    val selectTerm: N           = "selectTerm"
    val selectType: N           = "selectType"
    val self: N                 = "self"
    val seqToArray: N           = "seqToArray"
    val setAccessible: N        = "setAccessible"
    val setAnnotations: N       = "setAnnotations"
    val setSymbol: N            = "setSymbol"
    val setType: N              = "setType"
    val setTypeSignature: N     = "setTypeSignature"
    val standardInterpolator: N = "standardInterpolator"
    val staticClass : N         = "staticClass"
    val staticModule : N        = "staticModule"
    val staticPackage : N       = "staticPackage"
    val strictEquality: N       = "strictEquality"
    val synchronized_ : N       = "synchronized"
    val tag: N                  = "tag"
    val tail: N                 = "tail"
    val `then` : N              = "then"
    val this_ : N               = "this"
    val thisPrefix : N          = "thisPrefix"
    val throw_ : N              = "throw"
    val throws: N               = "throws"
    val toArray: N              = "toArray"
    val toList: N               = "toList"
    val toObjectArray : N       = "toObjectArray"
    val toSeq: N                = "toSeq"
    val toString_ : N           = "toString"
    val toTypeConstructor: N    = "toTypeConstructor"
    val tpe : N                 = "tpe"
    val tracked: N              = "tracked"
    val transparent : N         = "transparent"
    val tree : N                = "tree"
    val true_ : N               = "true"
    val tupled: N               = "tupled"
    val typedProductIterator: N = "typedProductIterator"
    val typeTagToManifest: N    = "typeTagToManifest"
    val unapply: N              = "unapply"
    val unapplySeq: N           = "unapplySeq"
    val unbox: N                = "unbox"
    val universe: N             = "universe"
    val unsafeNulls: N          = "unsafeNulls"
    val update: N               = "update"
    val updateDynamic: N        = "updateDynamic"
    val using: N                = "using"
    val value: N                = "value"
    val valueOf : N             = "valueOf"
    val fromOrdinal: N          = "fromOrdinal"
    val values: N               = "values"
    val view_ : N               = "view"
    val varargGetter : N        = "varargGetter"
    val wait_ : N               = "wait"
    val wildcardType: N         = "wildcardType"
    val withFilter: N           = "withFilter"
    val withFilterIfRefutable: N = "withFilterIfRefutable$"
    val WorksheetWrapper: N     = "WorksheetWrapper"
    val wrap: N                 = "wrap"
    val writeReplace: N         = "writeReplace"
    val readResolve: N          = "readResolve"
    val zero: N                 = "zero"
    val zip: N                  = "zip"
    val nothingRuntimeClass: N  = "scala.runtime.Nothing$"
    val nullRuntimeClass: N     = "scala.runtime.Null$"

    val synthSwitch: N          = "$synthSwitch"
    val _scope: N               = "$scope"

    val nothingClass: N         = "Nothing$"
    val nullClass: N            = "Null$"

    val falseModuleClassNames: Set[N] = Set(nothingClass, nullClass, nothingRuntimeClass, nullRuntimeClass)

    // unencoded operators
    object raw {
      final val AMP  : N  = "&"
      final val BANG : N  = "!"
      final val BAR  : N  = "|"
      final val DOLLAR: N = "$"
      final val GE: N     = ">="
      final val LAMBDA: N = "λ"
      final val LE: N     = "<="
      final val MINUS: N  = "-"
      final val NE: N     = "!="
      final val PLUS : N  = "+"
      final val SLASH: N  = "/"
      final val STAR : N  = "*"
      final val TILDE: N  = "~"

      // kind-projector compat symbols
      final val MINUS_STAR  : N = "-*"
      final val PLUS_STAR   : N = "+*"
      final val MINUS_USCORE: N = "-_"
      final val PLUS_USCORE : N = "+_"

      final val isUnary: Set[Name] = Set(MINUS, PLUS, TILDE, BANG)
    }

    object specializedTypeNames {
      final val Boolean: N = "Z"
      final val Byte: N    = "B"
      final val Char: N    = "C"
      final val Short: N   = "S"
      final val Int: N     = "I"
      final val Long: N    = "J"
      final val Float: N   = "F"
      final val Double: N  = "D"
      final val Void: N    = "V"
      final val Object: N  = "L"

      final val prefix: N = "$m"
      final val separator: N = "c"
      final val suffix: N = "$sp"
    }

    // value-conversion methods
    val toByte: N   = "toByte"
    val toShort: N  = "toShort"
    val toChar: N   = "toChar"
    val toInt: N    = "toInt"
    val toLong: N   = "toLong"
    val toFloat: N  = "toFloat"
    val toDouble: N = "toDouble"

    // primitive operation methods for structural types mostly
    // overlap with the above, but not for these two.
    val toCharacter: N = "toCharacter"
    val toInteger: N   = "toInteger"

    // ASCII names for operators
    val ADD      : N = "+"
    val AND      : N = "&"
    val ASR      : N = ">>"
    val DIV      : N = "/"
    val EQ       : N = "=="
    val EQL      : N = "="
    val GE       : N = ">="
    val GT       : N = ">"
    val HASHHASH : N = "##"
    val LE       : N = "<="
    val LSL      : N = "<<"
    val LSR      : N = ">>>"
    val LT       : N = "<"
    val MINUS    : N = "-"
    val MOD      : N = "%"
    val MUL      : N = "*"
    val NE       : N = "!="
    val OR       : N = "|"
    val PLUS     : N = ADD    // technically redundant, but ADD looks funny with MINUS
    val SUB      : N = MINUS  // ... as does SUB with PLUS
    val XOR      : N = "^"
    val ZAND     : N = "&&"
    val ZOR      : N = "||"
    val PUREARROW: N = "->"
    val PURECTXARROW: N = "?->"

    // unary operators
    val UNARY_PREFIX: N = "unary_"
    val UNARY_~ : N = "unary_~"
    val UNARY_+ : N = "unary_+"
    val UNARY_- : N = "unary_-"
    val UNARY_! : N = "unary_!"

    // Grouped here so Cleanup knows what tests to perform.
    val CommonOpNames: Set[Name]   = Set(OR, XOR, AND, EQ, NE)
    val ConversionNames: Set[Name] = Set(toByte, toChar, toDouble, toFloat, toInt, toLong, toShort)
    val BooleanOpNames: Set[Name]  = Set(ZOR, ZAND, UNARY_!) ++ CommonOpNames
    val NumberOpNames: Set[Name]   = (
         Set(ADD, SUB, MUL, DIV, MOD, LSL, LSR, ASR, LT, LE, GE, GT)
      ++ Set(UNARY_+, UNARY_-, UNARY_!)
      ++ ConversionNames
      ++ CommonOpNames
    )

    val add: N                    = "add"
    val complement: N             = "complement"
    val divide: N                 = "divide"
    val multiply: N               = "multiply"
    val negate: N                 = "negate"
    val positive: N               = "positive"
    val shiftLogicalRight: N      = "shiftLogicalRight"
    val shiftSignedLeft: N        = "shiftSignedLeft"
    val shiftSignedRight: N       = "shiftSignedRight"
    val subtract: N               = "subtract"
    val takeAnd: N                = "takeAnd"
    val takeConditionalAnd: N     = "takeConditionalAnd"
    val takeConditionalOr: N      = "takeConditionalOr"
    val takeModulo: N             = "takeModulo"
    val takeNot: N                = "takeNot"
    val takeOr: N                 = "takeOr"
    val takeXor: N                = "takeXor"
    val testEqual: N              = "testEqual"
    val testGreaterOrEqualThan: N = "testGreaterOrEqualThan"
    val testGreaterThan: N        = "testGreaterThan"
    val testLessOrEqualThan: N    = "testLessOrEqualThan"
    val testLessThan: N           = "testLessThan"
    val testNotEqual: N           = "testNotEqual"

    val isBoxedNumberOrBoolean: N = "isBoxedNumberOrBoolean"
    val isBoxedNumber: N = "isBoxedNumber"
  }

  class ScalaTermNames extends ScalaNames[TermName] {
    protected implicit def fromString(s: String): TermName = termName(s)

    def syntheticParamName(i: Int): TermName = (i: @switch) match {
      case 0  => x_0
      case 1  => x_1
      case 2  => x_2
      case 3  => x_3
      case 4  => x_4
      case 5  => x_5
      case 6  => x_6
      case 7  => x_7
      case 8  => x_8
      case 9  => x_9
      case _  => termName("x$" + i)
    }

    def productAccessorName(j: Int): TermName = (j: @switch) match {
      case 1  => nme._1
      case 2  => nme._2
      case 3  => nme._3
      case 4  => nme._4
      case 5  => nme._5
      case 6  => nme._6
      case 7  => nme._7
      case 8  => nme._8
      case 9  => nme._9
      case 10 => nme._10
      case 11 => nme._11
      case 12 => nme._12
      case 13 => nme._13
      case 14 => nme._14
      case 15 => nme._15
      case 16 => nme._16
      case 17 => nme._17
      case 18 => nme._18
      case 19 => nme._19
      case 20 => nme._20
      case 21 => nme._21
      case 22 => nme._22
      case _  => termName("_" + j)
    }

    def localDummyName(clazz: Symbol)(using Context): TermName =
      termName(str.LOCALDUMMY_PREFIX + clazz.name + ">")

    def newBitmapName(bitmapPrefix: TermName, n: Int): TermName = bitmapPrefix ++ n.toString

    def selectorName(n: Int): TermName = productAccessorName(n + 1)

    object primitive {
      val arrayApply: TermName  = "[]apply"
      val arrayUpdate: TermName = "[]update"
      val arrayLength: TermName = "[]length"
      val names: Set[Name] = Set(arrayApply, arrayUpdate, arrayLength)
    }

    def isPrimitiveName(name: Name): Boolean = primitive.names.contains(name)
  }

  class ScalaTypeNames extends ScalaNames[TypeName] {
    protected implicit def fromString(s: String): TypeName = typeName(s)

    def syntheticTypeParamName(i: Int): TypeName = "X" + i

    final val Conforms: TypeName = encode("<:<")

    final val Uninstantiated: TypeName = "?$"

    val JFunctionPrefix: Seq[TypeName] = (0 to 2).map(i => s"scala.runtime.java8.JFunction${i}")
    val JProcedure: Seq[TypeName] = (0 to 22).map(i => s"scala.runtime.function.JProcedure${i}")
  }

  abstract class JavaNames[N <: Name] extends DefinedNames[N] {
    final val ABSTRACTkw: N     = kw("abstract")
    final val ASSERTkw: N       = kw("assert")
    final val BOOLEANkw: N      = kw("boolean")
    final val BREAKkw: N        = kw("break")
    final val BYTEkw: N         = kw("byte")
    final val CASEkw: N         = kw("case")
    final val CATCHkw: N        = kw("catch")
    final val CHARkw: N         = kw("char")
    final val CLASSkw: N        = kw("class")
    final val CONSTkw: N        = kw("const")
    final val CONTINUEkw: N     = kw("continue")
    final val DEFAULTkw: N      = kw("default")
    final val DOkw: N           = kw("do")
    final val DOUBLEkw: N       = kw("double")
    final val ELSEkw: N         = kw("else")
    final val ENUMkw: N         = kw("enum")
    final val EXTENDSkw: N      = kw("extends")
    final val FALSEkw: N        = kw("false")
    final val FINALkw: N        = kw("final")
    final val FINALLYkw: N      = kw("finally")
    final val FLOATkw: N        = kw("float")
    final val FORkw: N          = kw("for")
    final val IFkw: N           = kw("if")
    final val GOTOkw: N         = kw("goto")
    final val IMPLEMENTSkw: N   = kw("implements")
    final val IMPORTkw: N       = kw("import")
    final val INSTANCEOFkw: N   = kw("instanceof")
    final val INTkw: N          = kw("int")
    final val INTERFACEkw: N    = kw("interface")
    final val LONGkw: N         = kw("long")
    final val NATIVEkw: N       = kw("native")
    final val NEWkw: N          = kw("new")
    final val PACKAGEkw: N      = kw("package")
    final val PRIVATEkw: N      = kw("private")
    final val PROTECTEDkw: N    = kw("protected")
    final val PUBLICkw: N       = kw("public")
    final val RETURNkw: N       = kw("return")
    final val SHORTkw: N        = kw("short")
    final val STATICkw: N       = kw("static")
    final val STRICTFPkw: N     = kw("strictfp")
    final val SUPERkw: N        = kw("super")
    final val SWITCHkw: N       = kw("switch")
    final val SYNCHRONIZEDkw: N = kw("synchronized")
    final val THISkw: N         = kw("this")
    final val THROWkw: N        = kw("throw")
    final val THROWSkw: N       = kw("throws")
    final val TRANSIENTkw: N    = kw("transient")
    final val TRUEkw: N         = kw("true")
    final val TRYkw: N          = kw("try")
    final val VOIDkw: N         = kw("void")
    final val VOLATILEkw: N     = kw("volatile")
    final val WHILEkw: N        = kw("while")

    final val RECORDid: N         = "record"
    final val VARid: N            = "var"
    final val YIELDid: N          = "yield"

    final val BoxedBoolean: N       = "java.lang.Boolean"
    final val BoxedByte: N          = "java.lang.Byte"
    final val BoxedCharacter: N     = "java.lang.Character"
    final val BoxedDouble: N        = "java.lang.Double"
    final val BoxedFloat: N         = "java.lang.Float"
    final val BoxedInteger: N       = "java.lang.Integer"
    final val BoxedLong: N          = "java.lang.Long"
    final val BoxedNumber: N        = "java.lang.Number"
    final val BoxedShort: N         = "java.lang.Short"
    final val Class: N              = "java.lang.Class"
    final val IOOBException: N      = "java.lang.IndexOutOfBoundsException"
    final val InvTargetException: N = "java.lang.reflect.InvocationTargetException"
    final val MethodAsObject: N     = "java.lang.reflect.Method"
    final val NPException: N        = "java.lang.NullPointerException"
    final val Object: N             = "java.lang.Object"
    final val String: N             = "java.lang.String"
    final val Throwable: N          = "java.lang.Throwable"

    final val ForName: N          = "forName"
    final val GetCause: N         = "getCause"
    final val GetClass: N         = "getClass"
    final val GetClassLoader: N   = "getClassLoader"
    final val GetComponentType: N = "getComponentType"
    final val GetMethod: N        = "getMethod"
    final val Invoke: N           = "invoke"
    final val JavaLang: N         = "java.lang"

    final val BeanProperty: N        = "scala.beans.BeanProperty"
    final val BooleanBeanProperty: N = "scala.beans.BooleanBeanProperty"
    final val JavaSerializable: N    = "java.io.Serializable"
  }



  class JavaTermNames extends JavaNames[TermName] {
    protected def fromString(s: String): TermName = termName(s)
  }
  class JavaTypeNames extends JavaNames[TypeName] {
    protected def fromString(s: String): TypeName = typeName(s)
  }

  val nme:    ScalaTermNames = new ScalaTermNames
  val tpnme:  ScalaTypeNames = new ScalaTypeNames
  val jnme:   JavaTermNames  = new JavaTermNames
  val jtpnme: JavaTypeNames  = new JavaTypeNames
}
