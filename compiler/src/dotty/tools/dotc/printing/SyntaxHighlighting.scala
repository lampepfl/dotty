package dotty.tools.dotc.printing

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Scanners.Scanner
import dotty.tools.dotc.parsing.Tokens._
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.diagnostic.MessageContainer
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.util.SourceFile

/** This object provides functions for syntax highlighting in the REPL */
object SyntaxHighlighting {

  /** if true, log erroneous positions being highlighted */
  private final val debug = true

  // Keep in sync with SyntaxHighlightingTests
  val NoColor         = Console.RESET
  val CommentColor    = Console.BLUE
  val KeywordColor    = Console.YELLOW
  val ValDefColor     = Console.CYAN
  val LiteralColor    = Console.RED
  val StringColor     = Console.GREEN
  val TypeColor       = Console.MAGENTA
  val AnnotationColor = Console.MAGENTA

  private class NoReporter extends Reporter {
    override def doReport(m: MessageContainer)(implicit ctx: Context): Unit = ()
  }

  def highlight(in: String)(implicit ctx: Context): String = {
    def freshCtx = ctx.fresh.setReporter(new NoReporter)
    if (in.isEmpty || ctx.settings.color.value == "never") in
    else {
      implicit val ctx = freshCtx
      val source = new SourceFile("<highlighting>", in.toCharArray)
      val colorAt = Array.fill(in.length)(NoColor)

      def highlightRange(from: Int, to: Int, color: String) =
        for (i <- from until to)
          colorAt(i) = color

      def highlightPosition(pos: Position, color: String) = if (pos.exists) {
        if (pos.start < 0 || pos.end > in.length) {
          if (debug)
            println(s"Trying to highlight erroneous position $pos. Input size: ${in.length}")
        }
        else
          highlightRange(pos.start, pos.end, color)
      }

      val scanner = new Scanner(source)
      while (scanner.token != EOF) {
        val start = scanner.offset
        val token = scanner.token
        val name = scanner.name
        scanner.nextToken()
        val end = scanner.lastOffset

        if (alphaKeywords.contains(token))
          highlightRange(start, end, KeywordColor)
        else if (token == IDENTIFIER && name == nme.???)
          highlightRange(start, end, Console.RED_B)
      }

      val treeHighlighter = new untpd.UntypedTreeTraverser {
        import untpd._

        def ignored(tree: NameTree) = {
          val name = tree.name.toTermName
          // trees named <error> and <init> have weird positions
          name == nme.ERROR || name == nme.CONSTRUCTOR
        }

        def traverse(tree: Tree)(implicit ctx: Context): Unit = {
          tree match {
            case tree: NameTree if ignored(tree) =>
              ()
            case tree: MemberDef /* ValOrDefDef | ModuleDef | TypeDef */ =>
              for (annotation <- tree.rawMods.annotations)
                highlightPosition(annotation.pos, AnnotationColor)
              val color = if (tree.isInstanceOf[ValOrDefDef]) ValDefColor else TypeColor
              highlightPosition(tree.namePos, color)
            case tree: Ident if tree.isType =>
              highlightPosition(tree.pos, TypeColor)
            case _: TypTree =>
              highlightPosition(tree.pos, TypeColor)
            case _: Literal =>
              highlightPosition(tree.pos, LiteralColor)
            case _ =>
          }
          traverseChildren(tree)
        }
      }

      val parser = new Parser(source)
      val trees = parser.blockStatSeq()
      for (tree <- trees)
        treeHighlighter.traverse(tree)

      val highlighted = new StringBuilder()

      for (idx <- colorAt.indices) {
        val prev = if (idx == 0) NoColor else colorAt(idx - 1)
        val curr = colorAt(idx)
        if (curr != prev)
          highlighted.append(curr)
        highlighted.append(in(idx))
      }

      if (colorAt.last != NoColor)
        highlighted.append(NoColor)

      highlighted.toString
    }
  }
}
