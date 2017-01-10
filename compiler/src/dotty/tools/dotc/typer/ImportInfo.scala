package dotty.tools
package dotc
package typer

import ast.{tpd, untpd}
import ast.Trees._
import core._
import util.SimpleMap
import Symbols._, Names._, Denotations._, Types._, Contexts._, StdNames._, Flags._
import Decorators.StringInterpolators

object ImportInfo {
  /** The import info for a root import from given symbol `sym` */
  def rootImport(refFn: () => TermRef)(implicit ctx: Context) = {
    val selectors = untpd.Ident(nme.WILDCARD) :: Nil
    def expr = tpd.Ident(refFn())
    def imp = tpd.Import(expr, selectors)
    new ImportInfo(imp.symbol, selectors, isRootImport = true)
  }
}

/** Info relating to an import clause
 *  @param   sym        The import symbol defined by the clause
 *  @param   selectors  The selector clauses
 *  @param   rootImport true if this is one of the implicit imports of scala, java.lang
 *                      or Predef in the start context, false otherwise.
 */
class ImportInfo(symf: => Symbol, val selectors: List[untpd.Tree], val isRootImport: Boolean = false)(implicit ctx: Context) {

  // Dotty deviation: we cannot use a lazy val here for the same reason
  // that we cannot use one for `DottyPredefModuleRef`.
  def sym = {
    if (_sym == null) {
      _sym = symf
      assert(_sym != null)
    }
    _sym
  }
  private[this] var _sym: Symbol = _

  /** The (TermRef) type of the qualifier of the import clause */
  def site(implicit ctx: Context): Type = {
    val ImportType(expr) = sym.info
    expr.tpe
  }

  /** The names that are excluded from any wildcard import */
  def excluded: Set[TermName] = { ensureInitialized(); myExcluded }

  /** A mapping from renamed to original names */
  def reverseMapping: SimpleMap[TermName, TermName] = { ensureInitialized(); myMapped }

  /** The original names imported by-name before renaming */
  def originals: Set[TermName] = { ensureInitialized(); myOriginals }

  /** Does the import clause end with wildcard? */
  def isWildcardImport = { ensureInitialized(); myWildcardImport }

  private var myExcluded: Set[TermName] = null
  private var myMapped: SimpleMap[TermName, TermName] = null
  private var myOriginals: Set[TermName] = null
  private var myWildcardImport: Boolean = false

  /** Compute info relating to the selector list */
  private def ensureInitialized(): Unit = if (myExcluded == null) {
    myExcluded = Set()
    myMapped = SimpleMap.Empty
    myOriginals = Set()
    def recur(sels: List[untpd.Tree]): Unit = sels match {
      case sel :: sels1 =>
        sel match {
          case Thicket(Ident(name: TermName) :: Ident(nme.WILDCARD) :: Nil) =>
            myExcluded += name
          case Thicket(Ident(from: TermName) :: Ident(to: TermName) :: Nil) =>
            myMapped = myMapped.updated(to, from)
            myExcluded += from
            myOriginals += from
          case Ident(nme.WILDCARD) =>
            myWildcardImport = true
          case Ident(name: TermName) =>
            myMapped = myMapped.updated(name, name)
            myOriginals += name
        }
        recur(sels1)
      case nil =>
    }
    recur(selectors)
  }

  /** The implicit references imported by this import clause */
  def importedImplicits: List[TermRef] = {
    val pre = site
    if (isWildcardImport) {
      val refs = pre.implicitMembers
      if (excluded.isEmpty) refs
      else refs filterNot (ref => excluded contains ref.name.toTermName)
    } else
      for {
        renamed <- reverseMapping.keys
        denot <- pre.member(reverseMapping(renamed)).altsWith(_ is Implicit)
      } yield TermRef.withSigAndDenot(pre, renamed, denot.signature, denot)
  }

  /** The root import symbol hidden by this symbol, or NoSymbol if no such symbol is hidden.
   *  Note: this computation needs to work even for un-initialized import infos, and
   *  is not allowed to force initialization.
   *
   *  TODO: Once we have fully bootstrapped, I would prefer if we expressed
   *  unimport with an `override` modifier, and generalized it to all imports.
   *  I believe this would be more transparent than the current set of conditions. E.g.
   *
   *      override import Predef.{any2stringAdd => _, StringAdd => _, _} // disables String +
   *      override import java.lang.{}                                   // disables all imports
   */
  lazy val unimported: Symbol = {
    lazy val sym = site.termSymbol
    val hasMaskingSelector = selectors exists {
      case Thicket(_ :: Ident(nme.WILDCARD) :: Nil) => true
      case _ => false
    }
    if (hasMaskingSelector && defn.RootImportTypes.exists(_.symbol == sym)) sym
    else NoSymbol
  }

  override def toString = {
    val siteStr = site.show
    val exprStr = if (siteStr endsWith ".type") siteStr dropRight 5 else siteStr
    val selectorStr = selectors match {
      case Ident(name) :: Nil => name.show
      case _ => "{...}"
    }
    i"import $exprStr.$selectorStr"
  }
}
