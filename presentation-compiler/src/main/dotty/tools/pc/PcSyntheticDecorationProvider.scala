package dotty.tools.pc


import java.nio.file.Paths

import scala.meta.internal.metals.ReportContext
import dotty.tools.pc.utils.MtagsEnrichments.*
import dotty.tools.pc.printer.ShortenedTypePrinter
import scala.meta.pc.SymbolSearch
import scala.meta.pc.SyntheticDecoration
import scala.meta.pc.SyntheticDecorationsParams
import scala.meta.internal.pc.DecorationKind
import scala.meta.internal.pc.Decoration


import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span

final class PcSyntheticDecorationsProvider(
    driver: InteractiveDriver,
    params: SyntheticDecorationsParams,
    symbolSearch: SymbolSearch,
)(using ReportContext):

  val uri = params.uri().nn
  val filePath = Paths.get(uri).nn
  val sourceText = params.text().nn
  val text = sourceText.toCharArray().nn
  val source =
    SourceFile.virtual(filePath.toString, sourceText)
  driver.run(uri, source)

  given InferredType.Text = InferredType.Text(text)
  given ctx: Context = driver.currentCtx
  val unit = driver.currentCtx.run.nn.units.head

  def tpdTree = unit.tpdTree

  def provide(): List[SyntheticDecoration] =
    val deepFolder = DeepFolder[Synthetics](collectDecorations)
    deepFolder(Synthetics.empty, tpdTree).result()

  def collectDecorations(
      decorations: Synthetics,
      tree: Tree,
  ): Synthetics =
    tree match
      case ImplicitConversion(name, range) if params.implicitConversions() =>
        val adjusted = range.adjust(text)._1
        decorations
          .add(
            Decoration(
              adjusted.startPos.toLsp,
              name + "(",
              DecorationKind.ImplicitConversion,
            )
          )
          .add(
            Decoration(
              adjusted.endPos.toLsp,
              ")",
              DecorationKind.ImplicitConversion,
            )
          )
      case ImplicitParameters(names, pos, allImplicit)
          if params.implicitParameters() =>
        val label =
          if allImplicit then names.mkString("(", ", ", ")")
          else names.mkString(", ", ", ", "")
        decorations.add(
          Decoration(
            pos.adjust(text)._1.toLsp,
            label,
            DecorationKind.ImplicitParameter,
          )
        )
      case TypeParameters(tpes, pos, sel)
          if params.typeParameters() && !syntheticTupleApply(sel) =>
        val label = tpes.map(toLabel(_, pos)).mkString("[", ", ", "]")
        decorations.add(
          Decoration(
            pos.adjust(text)._1.endPos.toLsp,
            label,
            DecorationKind.TypeParameter,
          )
        )
      case InferredType(tpe, pos, defTree) if params.inferredTypes() =>
        val adjustedPos = pos.adjust(text)._1.endPos
        if decorations.containsDef(adjustedPos.start) then decorations
        else
          decorations.add(
            Decoration(
              adjustedPos.toLsp,
              ": " + toLabel(tpe, pos),
              DecorationKind.InferredType,
            ),
            adjustedPos.start,
          )
      case _ => decorations

  private def toLabel(
      tpe: Type,
      pos: SourcePosition,
  ): String =
    val tpdPath =
      Interactive.pathTo(unit.tpdTree, pos.span)

    val indexedCtx = IndexedContext(Interactive.contextOfPath(tpdPath))
    val printer = ShortenedTypePrinter(
      symbolSearch
    )(using indexedCtx)
    def optDealias(tpe: Type): Type =
      def isInScope(tpe: Type): Boolean =
        tpe match
          case tref: TypeRef =>
            indexedCtx.lookupSym(
              tref.currentSymbol
            ) == IndexedContext.Result.InScope
          case AppliedType(tycon, args) =>
            isInScope(tycon) && args.forall(isInScope)
          case _ => true
      if isInScope(tpe)
      then tpe
      else tpe.metalsDealias(using indexedCtx.ctx)

    val dealiased = optDealias(tpe)
    printer.tpe(dealiased)
  end toLabel

  private val definitions = IndexedContext(ctx).ctx.definitions
  private def syntheticTupleApply(tree: Tree): Boolean =
    tree match
      case sel: Select =>
        if definitions.isTupleNType(sel.symbol.info.finalResultType) then
          sel match
            case Select(tupleClass: Ident, _)
                if !tupleClass.span.isZeroExtent &&
                  tupleClass.span.exists &&
                  tupleClass.name.startsWith("Tuple") =>
              val pos = tupleClass.sourcePos
              !sourceText.slice(pos.start, pos.end).mkString.startsWith("Tuple")
            case _ => true
        else false
      case _ => false
end PcSyntheticDecorationsProvider

object ImplicitConversion:
  def unapply(tree: Tree)(using Context) =
    tree match
      case Apply(fun: Ident, args) if isSynthetic(fun) =>
        implicitConversion(fun, args)
      case Apply(Select(fun, name), args)
          if name == nme.apply && isSynthetic(fun) =>
        implicitConversion(fun, args)
      case _ => None
  private def isSynthetic(tree: Tree)(using Context) =
    tree.span.isSynthetic && tree.symbol.isOneOf(Flags.GivenOrImplicit)

  private def implicitConversion(fun: Tree, args: List[Tree])(using Context) =
    val lastArgPos =
      args.lastOption.map(_.sourcePos).getOrElse(fun.sourcePos)
    Some(
      fun.symbol.decodedName,
      lastArgPos.withStart(fun.sourcePos.start),
    )
end ImplicitConversion

object ImplicitParameters:
  def unapply(tree: Tree)(using Context) =
    tree match
      case Apply(fun, args)
          if args.exists(isSyntheticArg) && !tree.sourcePos.span.isZeroExtent =>
        val (implicitArgs, providedArgs) = args.partition(isSyntheticArg)
        val allImplicit = providedArgs.isEmpty
        val pos = implicitArgs.head.sourcePos
        Some(implicitArgs.map(_.symbol.decodedName), pos, allImplicit)
      case Apply(ta @ TypeApply(fun, _), _)
          if fun.span.isSynthetic && isValueOf(fun) =>
        Some(
          List("new " + tpnme.valueOf.decoded.capitalize + "(...)"),
          fun.sourcePos,
          true,
        )
      case _ => None
  private def isValueOf(tree: Tree)(using Context) =
    val symbol = tree.symbol.maybeOwner
    symbol.name.decoded == tpnme.valueOf.decoded.capitalize
  private def isSyntheticArg(tree: Tree)(using Context) = tree match
    case tree: Ident =>
      tree.span.isSynthetic && tree.symbol.isOneOf(Flags.GivenOrImplicit)
    case _ => false
end ImplicitParameters

object TypeParameters:
  def unapply(tree: Tree)(using Context) =
    tree match
      case TypeApply(sel: Select, _) if sel.isForComprehensionMethod => None
      case TypeApply(fun, args) if inferredTypeArgs(args) =>
        val pos = fun match
          case sel: Select if sel.isInfix =>
            sel.sourcePos.withEnd(sel.nameSpan.end)
          case _ => fun.sourcePos
        val tpes = args.map(_.typeOpt.stripTypeVar.widen.finalResultType)
        Some((tpes, pos.endPos, fun))
      case _ => None
  private def inferredTypeArgs(args: List[Tree]): Boolean =
    args.forall {
      case tt: TypeTree if tt.span.exists && !tt.span.isZeroExtent => true
      case _ => false
    }
end TypeParameters

object InferredType:
  opaque type Text = Array[Char]
  object Text:
    def apply(text: Array[Char]): Text = text

  def unapply(tree: Tree)(using text: Text, cxt: Context) =
    tree match
      case vd @ ValDef(_, tpe, _)
          if isValidSpan(tpe.span, vd.nameSpan) &&
            !vd.symbol.is(Flags.Enum) &&
            !isValDefBind(text, vd) =>
        if vd.symbol == vd.symbol.sourceSymbol then
          Some(tpe.typeOpt, tpe.sourcePos.withSpan(vd.nameSpan), vd)
        else None
      case vd @ DefDef(_, _, tpe, _)
          if isValidSpan(tpe.span, vd.nameSpan) &&
            tpe.span.start >= vd.nameSpan.end &&
            !vd.symbol.isConstructor &&
            !vd.symbol.is(Flags.Mutable) =>
        if vd.symbol == vd.symbol.sourceSymbol then
          Some(tpe.typeOpt, tpe.sourcePos, vd)
        else None
      case bd @ Bind(
            name,
            Ident(nme.WILDCARD),
          ) =>
        Some(bd.symbol.info, bd.namePos, bd)
      case _ => None

  private def isValidSpan(tpeSpan: Span, nameSpan: Span): Boolean =
    tpeSpan.isZeroExtent &&
      nameSpan.exists &&
      !nameSpan.isZeroExtent

  /* If is left part of val definition bind:
   * val <<t>> @ ... =
   */
  def isValDefBind(text: Text, vd: ValDef)(using Context) =
    val afterDef = text.drop(vd.nameSpan.end)
    val index = afterDef.indexAfterSpacesAndComments
    index >= 0 && index < afterDef.size && afterDef(index) == '@'

end InferredType

case class Synthetics(
    decorations: List[Decoration],
    definitions: Set[Int],
):
  def containsDef(offset: Int) = definitions(offset)
  def add(decoration: Decoration, offset: Int) =
    copy(
      decorations = addDecoration(decoration),
      definitions = definitions + offset,
    )
  def add(decoration: Decoration) =
    copy (
      decorations = addDecoration(decoration)
    )

  // If method has both type parameter and implicit parameter, we want the type parameter decoration to be displayed first,
  // but it's added second. This method adds the decoration to the right position in the list.
  private def addDecoration(decoration: Decoration): List[Decoration] =
    val atSamePos =
      decorations.takeWhile(_.range.getStart() == decoration.range.getStart())
    (atSamePos :+ decoration) ++ decorations.drop(atSamePos.size)

  def result(): List[Decoration] = decorations.reverse
end Synthetics

object Synthetics:
  def empty: Synthetics = Synthetics(Nil, Set.empty)
