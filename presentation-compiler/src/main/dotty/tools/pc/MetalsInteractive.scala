package dotty.tools
package pc

import scala.annotation.tailrec

import dotc.*
import ast.*, tpd.*
import core.*, Contexts.*, Flags.*, Names.*, Symbols.*, Types.*
import interactive.*
import util.*
import util.SourcePosition

object MetalsInteractive:

  def contextOfStat(
      stats: List[Tree],
      stat: Tree,
      exprOwner: Symbol,
      ctx: Context
  ): Context = stats match
    case Nil =>
      ctx
    case first :: _ if first eq stat =>
      ctx.exprContext(stat, exprOwner)
    case (imp: Import) :: rest =>
      contextOfStat(
        rest,
        stat,
        exprOwner,
        ctx.importContext(imp, inContext(ctx) { imp.symbol })
      )
    case _ :: rest =>
      contextOfStat(rest, stat, exprOwner, ctx)

  /**
   * Check if the given `sourcePos` is on the name of enclosing tree.
   * ```
   * // For example, if the postion is on `foo`, returns true
   * def foo(x: Int) = { ... }
   *      ^
   *
   * // On the other hand, it points to non-name position, return false.
   * def foo(x: Int) = { ... }
   *  ^
   * ```
   * @param path - path to the position given by `Interactive.pathTo`
   */
  def isOnName(
      path: List[Tree],
      sourcePos: SourcePosition,
      source: SourceFile
  )(using Context): Boolean =
    def contains(tree: Tree): Boolean = tree match
      case select: Select =>
        // using `nameSpan` as SourceTree for Select (especially symbolic-infix e.g. `::` of `1 :: Nil`) miscalculate positions
        select.nameSpan.contains(sourcePos.span)
      case tree: Ident =>
        tree.sourcePos.contains(sourcePos)
      case tree: NamedDefTree =>
        tree.namePos.contains(sourcePos)
      case tree: NameTree =>
        SourceTree(tree, source).namePos.contains(sourcePos)
      // TODO: check the positions for NamedArg and Import
      case _: NamedArg => true
      case _: Import => true
      case app: (Apply | TypeApply) => contains(app.fun)
      case _ => false
    end contains

    val enclosing = path
      .dropWhile(t => !t.symbol.exists && !t.isInstanceOf[NamedArg])
      .headOption
      .getOrElse(EmptyTree)
    contains(enclosing)
  end isOnName

  private lazy val isForName: Set[Name] = Set[Name](
    StdNames.nme.map,
    StdNames.nme.withFilter,
    StdNames.nme.flatMap,
    StdNames.nme.foreach
  )
  def isForSynthetic(gtree: Tree)(using Context): Boolean =
    def isForComprehensionSyntheticName(select: Select): Boolean =
      select.sourcePos.toSynthetic == select.qualifier.sourcePos.toSynthetic && isForName(
        select.name
      )
    gtree match
      case Apply(fun, List(_: Block)) => isForSynthetic(fun)
      case TypeApply(fun, _) => isForSynthetic(fun)
      case gtree: Select if isForComprehensionSyntheticName(gtree) => true
      case _ => false

  def enclosingSymbols(
      path: List[Tree],
      pos: SourcePosition,
      indexed: IndexedContext,
      skipCheckOnName: Boolean = false
  ): List[Symbol] =
    enclosingSymbolsWithExpressionType(path, pos, indexed, skipCheckOnName)
      .map(_._1)

  /**
   * Returns the list of tuple enclosing symbol and
   * the symbol's expression type if possible.
   */
  @tailrec
  def enclosingSymbolsWithExpressionType(
      path: List[Tree],
      pos: SourcePosition,
      indexed: IndexedContext,
      skipCheckOnName: Boolean = false
  ): List[(Symbol, Type)] =
    import indexed.ctx
    path match
      // For a named arg, find the target `DefDef` and jump to the param
      case NamedArg(name, _) :: Apply(fn, _) :: _ =>
        val funSym = fn.symbol
        if funSym.is(Synthetic) && funSym.owner.is(CaseClass) then
          val sym = funSym.owner.info.member(name).symbol
          List((sym, sym.info))
        else
          val paramSymbol =
            for param <- funSym.paramSymss.flatten.find(_.name == name)
            yield param
          val sym = paramSymbol.getOrElse(fn.symbol)
          List((sym, sym.info))

      case (_: untpd.ImportSelector) :: (imp: Import) :: _ =>
        importedSymbols(imp, _.span.contains(pos.span)).map(sym =>
          (sym, sym.info)
        )

      case (imp: Import) :: _ =>
        importedSymbols(imp, _.span.contains(pos.span)).map(sym =>
          (sym, sym.info)
        )

      // wildcard param
      case head :: _ if (head.symbol.is(Param) && head.symbol.is(Synthetic)) =>
        List((head.symbol, head.typeOpt))

      case (head @ Select(target, name)) :: _
          if head.symbol.is(Synthetic) && name == StdNames.nme.apply =>
        val sym = target.symbol
        if sym.is(Synthetic) && sym.is(Module) then
          List((sym.companionClass, sym.companionClass.info))
        else List((target.symbol, target.typeOpt))

      // L@@ft(...)
      case (head @ ApplySelect(select)) :: _
          if select.qualifier.sourcePos.contains(pos) &&
            select.name == StdNames.nme.apply =>
        List((head.symbol, head.typeOpt))

      // for Inlined we don't have a symbol, but it's needed to show proper type
      case (head @ Inlined(call, bindings, expansion)) :: _ =>
        List((call.symbol, head.typeOpt))

      // for comprehension
      case (head @ ApplySelect(select)) :: _ if isForSynthetic(head) =>
        // If the cursor is on the qualifier, return the symbol for it
        // `for { x <- List(1).head@@Option }`  returns the symbol of `headOption`
        if select.qualifier.sourcePos.contains(pos) then
          List((select.qualifier.symbol, select.qualifier.typeOpt))
        // Otherwise, returns the symbol of for synthetics such as "withFilter"
        else List((head.symbol, head.typeOpt))

      // f@@oo.bar
      case Select(target, _) :: _
          if target.span.isSourceDerived &&
            target.sourcePos.contains(pos) =>
        List((target.symbol, target.typeOpt))

      /* In some cases type might be represented by TypeTree, however it's possible
       * that the type tree will not be marked properly as synthetic even if it doesn't
       * exist in the code.
       *
       * For example for `Li@@st(1)` we will get the type tree representing [Int]
       * despite it not being in the code.
       *
       * To work around it we check if the current and parent spans match, if they match
       * this most likely means that the type tree is synthetic, since it has efectively
       * span of 0.
       */
      case (tpt: TypeTree) :: parent :: _
          if tpt.span != parent.span && !tpt.symbol.is(Synthetic) =>
        List((tpt.symbol, tpt.typeOpt))

      /* TypeTest class https://dotty.epfl.ch/docs/reference/other-new-features/type-test.html
       * compiler automatically adds unapply if possible, we need to find the type symbol
       */
      case (head @ CaseDef(pat, _, _)) :: _
          if pat.symbol.exists && defn.TypeTestClass == pat.symbol.owner =>
        pat match
          case UnApply(fun, _, pats) =>
            val tpeSym = pats.head.typeOpt.typeSymbol
            List((tpeSym, tpeSym.info))
          case _ =>
            Nil

      case path @ head :: tail =>
        if head.symbol.is(Exported) then
          val sym = head.symbol.sourceSymbol
          List((sym, sym.info))
        else if head.symbol.is(Synthetic) then
          enclosingSymbolsWithExpressionType(
            tail,
            pos,
            indexed,
            skipCheckOnName
          )
        else if head.symbol != NoSymbol then
          if skipCheckOnName ||
            MetalsInteractive.isOnName(
              path,
              pos,
              indexed.ctx.source
            )
          then List((head.symbol, head.typeOpt))
          /* Type tree for List(1) has an Int type variable, which has span
           * but doesn't exist in code.
           * https://github.com/lampepfl/dotty/issues/15937
           */
          else if head.isInstanceOf[TypeTree] then
            enclosingSymbolsWithExpressionType(tail, pos, indexed)
          else Nil
        else
          val recovered = recoverError(head, indexed)
          if recovered.isEmpty then
            enclosingSymbolsWithExpressionType(
              tail,
              pos,
              indexed,
              skipCheckOnName
            )
          else recovered.map(sym => (sym, sym.info))
        end if
      case Nil => Nil
    end match
  end enclosingSymbolsWithExpressionType

  import dotty.tools.pc.utils.MtagsEnrichments.*

  private def recoverError(
      tree: Tree,
      indexed: IndexedContext
  ): List[Symbol] =
    import indexed.ctx

    tree match
      case select: Select =>
        select.qualifier.typeOpt
          .member(select.name)
          .allSymbols
          .filter(_ != NoSymbol)
      case ident: Ident => indexed.findSymbol(ident.name).toList.flatten
      case _ => Nil
  end recoverError

  object ApplySelect:
    def unapply(tree: Tree): Option[Select] = Option(tree).collect {
      case select: Select => select
      case Apply(select: Select, _) => select
      case Apply(TypeApply(select: Select, _), _) => select
    }
  end ApplySelect

  object TreeApply:
    def unapply(tree: Tree): Option[(Tree, List[Tree])] =
      tree match
        case TypeApply(qual, args) => Some(qual -> args)
        case Apply(qual, args) => Some(qual -> args)
        case UnApply(qual, implicits, args) => Some(qual -> (implicits ++ args))
        case AppliedTypeTree(qual, args) => Some(qual -> args)
        case _ => None
end MetalsInteractive
