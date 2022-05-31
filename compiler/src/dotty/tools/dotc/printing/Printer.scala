package dotty.tools
package dotc
package printing

import core._
import Texts._, ast.Trees._
import Types.{Type, SingletonType, LambdaParam},
       Symbols.Symbol, Scopes.Scope, Constants.Constant,
       Names.Name, Denotations._, Annotations.Annotation
import typer.Implicits.SearchResult
import util.SourcePosition
import typer.ImportInfo

import scala.annotation.internal.sharable

/** The base class of all printers
 */
abstract class Printer {

  private var prec: Precedence = GlobalPrec

  /** The current precedence level.
   *  When pretty-printing arguments of operator `op`, `currentPrecedence` must equal `op`'s precedence level,
   *  so that pretty-printing expressions using lower-precedence operators can insert parentheses automatically
   *  by calling `changePrec`.
   */
  def currentPrecedence: Precedence = prec

  /** Generate text using `op`, assuming a given precedence level `prec`.
   *
   *  ### `atPrec` vs `changePrec`
   *
   *  This is to be used when changing precedence inside some sort of parentheses:
   *  for instance, to print T[A]` use
   *  `toText(T) ~ '[' ~ atPrec(GlobalPrec) { toText(A) } ~ ']'`.
   *
   *  If the presence of the parentheses depends on precedence, inserting them manually is most certainly a bug.
   *  Use `changePrec` instead to generate them exactly when needed.
   */
  def atPrec(prec: Precedence)(op: => Text): Text = {
    val outerPrec = this.prec
    this.prec = prec
    try op
    finally this.prec = outerPrec
  }

  /** Generate text using `op`, assuming a given precedence level `prec`.
   *  If new level `prec` is lower than previous level, put text in parentheses.
   *
   *  ### `atPrec` vs `changePrec`
   *
   *  To pretty-print `A op B`, you need something like
   *  `changePrec(parsing.precedence(op, isType)) { toText(a) ~ op ~ toText(b) }` // BUGGY
   *  that will insert parentheses around `A op B` if, for instance, the
   *  preceding operator has higher precedence.
   *
   *  But that does not handle infix operators with left- or right- associativity.
   *
   *  If op and op' have the same precedence and associativity,
   *  A op B op' C parses as (A op B) op' C if op and op' are left-associative, and as
   *  A op (B op' C) if they're right-associative, so we need respectively
   *  ```scala
   *  val isType = ??? // is this a term or type operator?
   *  val prec = parsing.precedence(op, isType)
   *  // either:
   *  changePrec(prec) { toText(a) ~ op ~ atPrec(prec + 1) { toText(b) } } // for left-associative op and op'
   *  // or:
   *  changePrec(prec) { atPrec(prec + 1) { toText(a) } ~ op ~ toText(b) } // for right-associative op and op'
   *  ```
   */
  def changePrec(prec: Precedence)(op: => Text): Text =
    if (prec < this.prec) atPrec(prec) ("(" ~ op ~ ")") else atPrec(prec)(op)

  /** The name, possibly with with namespace suffix if debugNames is set:
   *  /L for local names, /V for other term names, /T for type names
   */
  def nameString(name: Name): String

  /** The name of the given symbol.
   *  If !settings.debug, the original name where
   *  expansions of operators are translated back to operator symbol.
   *  E.g. $eq => =.
   *  If settings.uniqid, adds id.
   */
  def nameString(sym: Symbol): String

  /** The fully qualified name of the symbol */
  def fullNameString(sym: Symbol): String

  /** The kind of the symbol */
  def kindString(sym: Symbol): String

  /** The name as a text */
  def toText(name: Name): Text

  /** Textual representation, including symbol's kind e.g., "class Foo", "method Bar".
   *  If hasMeaninglessName is true, uses the owner's name to disambiguate identity.
   */
  def toText(sym: Symbol): Text

  /** Textual representation of singleton type reference */
  def toTextRef(tp: SingletonType): Text

  /** Textual representation of a prefix of some reference, ending in `.` or `#` */
  def toTextPrefix(tp: Type): Text

  /** Textual representation of symbol's declaration */
  def dclText(sym: Symbol): Text

  /** Textual representation of single denotation's declaration */
  def dclText(sd: SingleDenotation): Text

  /** If symbol's owner is a printable class C, the text "in C", otherwise "" */
  def locationText(sym: Symbol): Text

  /** Textual representation of symbol and its location */
  def locatedText(sym: Symbol): Text

  /** A description of sym's location */
  def extendedLocationText(sym: Symbol): Text

  /** Textual description of regular annotation in terms of its tree */
  def annotText(annot: Annotation): Text

  /** Textual representation of denotation */
  def toText(denot: Denotation): Text

  /** Textual representation of constant */
  def toText(const: Constant): Text

  /** Textual representation of annotation */
  def toText(annot: Annotation): Text

  /** Textual representation of type */
  def toText(tp: Type): Text

  /** Textual representation of lambda param */
  def toText(tree: LambdaParam): Text

  /** Textual representation of all symbols in given list,
   *  using `dclText` for displaying each.
   */
  def dclsText(syms: List[Symbol], sep: String = "\n"): Text

  /** Textual representation of all definitions in a scope using `dclText` for each */
  def toText(sc: Scope): Text

  /** Textual representation of tree */
  def toText[T >: Untyped](tree: Tree[T]): Text

  /** Textual representation of source position */
  def toText(pos: SourcePosition): Text

  /** Textual representation of implicit search result */
  def toText(result: SearchResult): Text

  /** Textual representation of info relating to an import clause */
  def toText(result: ImportInfo): Text

  /** Textual representation of a constraint */
  def toText(c: OrderingConstraint): Text

  /** Render element within highest precedence */
  def toTextLocal(elem: Showable): Text =
    atPrec(DotPrec) { elem.toText(this) }

  /** Render element within lowest precedence */
  def toTextGlobal(elem: Showable): Text =
    atPrec(GlobalPrec) { elem.toText(this) }

  /** Render elements alternating with `sep` string */
  def toText(elems: Iterable[Showable], sep: String): Text =
    Text(elems map (_ toText this), sep)

  /** Render elements within highest precedence */
  def toTextLocal(elems: Iterable[Showable], sep: String): Text =
    atPrec(DotPrec) { toText(elems, sep) }

  /** Render elements within lowest precedence */
  def toTextGlobal(elems: Iterable[Showable], sep: String): Text =
    atPrec(GlobalPrec) { toText(elems, sep) }

  /** A plain printer without any embellishments */
  def plain: Printer
}
object Printer {

  /** Debug hook; set to true if you want to see unique ids but cannot run with option
   *  -uniqid. A typical use case is for further exploration after a -Ytest-pickler failure.
   */
  @sharable var debugPrintUnique: Boolean = false
}
