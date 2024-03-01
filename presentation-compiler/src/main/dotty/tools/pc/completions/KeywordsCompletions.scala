package dotty.tools.pc.completions

import scala.collection.mutable.ListBuffer
import scala.meta.internal.pc.Keyword

import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.ast.Positioned
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.untpd.UntypedTreeTraverser
import dotty.tools.dotc.core.Comments
import dotty.tools.dotc.core.Comments.Comment
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourcePosition

object KeywordsCompletions:

  def contribute(
      path: List[Tree],
      completionPos: CompletionPos,
      comments: List[Comment]
  )(using ctx: Context): List[CompletionValue] =
    lazy val notInComment =
      checkIfNotInComment(completionPos.originalCursorPosition, comments)

    path match
      case Nil | (_: PackageDef) :: _ if completionPos.query.isEmpty() =>
        Keyword.all.collect {
          // topelevel definitions are allowed in Scala 3
          case kw if (kw.isPackage || kw.isTemplate) && notInComment =>
            CompletionValue.keyword(kw.name, kw.insertText)
        }
      case _ =>
        val isExpression = this.isExpression(path)
        val isBlock = this.isBlock(path)
        val isDefinition = this.isDefinition(path, completionPos.query, completionPos.originalCursorPosition)
        val isMethodBody = this.isMethodBody(path)
        val isTemplate = this.isTemplate(path)
        val isPackage = this.isPackage(path)
        val isParam = this.isParam(path)
        val isSelect = this.isSelect(path)
        val isImport = this.isImport(path)
        val possibleTemplateKeywords =
          checkTemplateForNewParents(enclosing = path, completionPos)

        Keyword.all.collect {
          case kw
              if kw.matchesPosition(
                completionPos.query,
                isExpression = isExpression,
                isBlock = isBlock,
                isDefinition = isDefinition,
                isMethodBody = isMethodBody,
                isTemplate = isTemplate,
                isPackage = isPackage,
                isParam = isParam,
                isScala3 = true,
                isSelect = isSelect,
                isImport = isImport,
                allowToplevel = true,
                canBeExtended = possibleTemplateKeywords.`extends`,
                canDerive = possibleTemplateKeywords.`derives`,
                hasExtend = possibleTemplateKeywords.`with`
              ) && notInComment =>
            CompletionValue.keyword(kw.name, kw.insertText)
        }
    end match
  end contribute

  private def checkIfNotInComment(
      pos: SourcePosition,
      comments: List[Comment]
  ): Boolean =
    !comments.exists(_.span.contains(pos.span))

  private def isPackage(enclosing: List[Tree]): Boolean =
    enclosing match
      case Nil | (_: PackageDef) :: _ => true
      case _ => false

  private def isParam(enclosing: List[Tree]): Boolean =
    enclosing match
      case (vd: ValDef) :: (dd: DefDef) :: _
          if dd.paramss.exists(pc => pc.contains(vd) && pc.size == 1) =>
        true
      case _ => false

  private def isTemplate(enclosing: List[Tree]): Boolean =
    enclosing match
      case Ident(_) :: (_: Template) :: _ => true
      case Ident(_) :: (_: ValOrDefDef) :: _ => true
      case (_: TypeDef) :: _ => true
      case _ => false

  private def isMethodBody(enclosing: List[Tree]): Boolean =
    enclosing match
      case Ident(_) :: (_: DefDef) :: _ => true
      case _ => false

  private def isSelect(enclosing: List[Tree]): Boolean =
    enclosing match
      case (_: Apply) :: (_: Select) :: _ => true
      case (_: Select) :: _ => true
      case _ => false

  private def isImport(enclosing: List[Tree]): Boolean =
    enclosing match
      case Import(_, _) :: _ => true
      case _ => false

  private def isDefinition(
      enclosing: List[Tree],
      name: String,
      pos: SourcePosition
  )(using ctx: Context): Boolean =
    enclosing match
      case (_: Ident) :: _ => false
      case _ =>
        // NOTE(olafur) in positions like "implicit obje@@" the parser discards the entire
        // statement and `enclosing` is not helpful. In these situations we fallback to the
        // diagnostics reported by the parser to see if it expected a definition here.
        // This is admittedly not a great solution, but it's the best I can think of at this point.
        val point = pos.withSpan(pos.span.withPoint(pos.point - name.length()))

        val isExpectedStartOfDefinition =
          ctx.reporter.allErrors.exists { info =>
            info.pos.focus == point &&
            info.message == "expected start of definition"
          }
        isExpectedStartOfDefinition

  private def isBlock(enclosing: List[Tree]): Boolean =
    enclosing match
      case Ident(_) :: Block(_, _) :: _ => true
      case _ => false

  private def isExpression(enclosing: List[Tree]): Boolean =
    enclosing match
      case Ident(_) :: (_: Template) :: _ => true
      case Ident(_) :: (_: ValOrDefDef) :: _ => true
      case Ident(_) :: t :: _ if t.isTerm => true
      case other => false

  case class TemplateKeywordAvailability(
      `extends`: Boolean,
      `with`: Boolean,
      `derives`: Boolean
  )

  object TemplateKeywordAvailability:
    def default = TemplateKeywordAvailability(false, false, false)

  /*
   * Checks whether given path and position can be followed with
   * `extends`, `with` or `derives` keywords.
   * @param enclosing - typed path to position
   * @param pos - completion position
   *
   * @returns TemplateKeywordAvailability with available keywords
   *
   * @note This method requires a typed path. The rest of the metals functionalities.
   */
  def checkTemplateForNewParents(enclosing: List[Tree], pos: CompletionPos)(
      using ctx: Context
  ): TemplateKeywordAvailability =

    def collectTypeAndModuleDefs(
      tree: untpd.Tree,
      f: PartialFunction[untpd.Tree, Boolean]
    )(using Context): List[untpd.Tree] = {
      val buf = ListBuffer.empty[untpd.Tree]
      val traverser = new UntypedTreeTraverser:
        def traverse(tree: untpd.Tree)(using Context) =
          foldOver(if f(tree) then buf += tree, tree)
      traverser.traverse(tree)
      buf.toList
    }

    /*
     * Finds tree which ends just before cursor positions, that may be extended or derive.
     * In Scala 3, such tree must be a `TypeDef` which has field of type `Template` describing
     * its parents and derived classes.
     *
     * @tree - enclosing tree
     *
     * @returns TypeDef tree defined before the cursor position or `enclosingTree` otherwise
     */
    def findLastSatisfyingTree(untpdPath: List[Positioned]): Option[untpd.Tree] =
      untpdPath.headOption.flatMap {
        case untpdTree: untpd.Tree =>
          collectTypeAndModuleDefs(untpdTree, {
            case typeDef: (untpd.TypeDef | untpd.ModuleDef) =>
              typeDef.span.exists && typeDef.span.end < pos.queryStart
            case _ => false
          })
          .filter(tree => tree.span.exists && tree.span.end < pos.queryStart)
          .maxByOption(_.span.end)
        case _ => None
      }

    def checkForPossibleKeywords(
        template: Template
    ): TemplateKeywordAvailability =
      TemplateKeywordAvailability(
        template.parents.isEmpty,
        template.parents.nonEmpty,
        template.derived.isEmpty
      )

    val untpdPath = NavigateAST.untypedPath(pos.originalCursorPosition.span)

    findLastSatisfyingTree(untpdPath).orElse { enclosing match
      case (typeDef: TypeDef) :: _ if typeDef.symbol.isEnumClass => untpdPath.headOption
      case _ => None
    }.map {
      case untpd.TypeDef(_, template: Template) => checkForPossibleKeywords(template)
      case untpd.ModuleDef(_, template: Template) => checkForPossibleKeywords(template)
      case template: Template => checkForPossibleKeywords(template)
    }.getOrElse(TemplateKeywordAvailability.default)


  end checkTemplateForNewParents

end KeywordsCompletions
