package dotty.tools.dotc.interactive

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names.{Name, termName}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{Symbol, defn}
import dotty.tools.dotc.core.TypeError
import dotty.tools.dotc.util.Chars.{isOperatorPart, isScalaLetter}
import dotty.tools.dotc.util.SourcePosition

object CustomCompletion {

  def completions(
    pos: SourcePosition,
    dependencyCompleteOpt: Option[String => (Int, Seq[String])],
    enableDeep: Boolean
  )(using Context): (Int, List[Completion]) = {
    val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, pos.span)
    computeCompletions(pos, path, dependencyCompleteOpt, enableDeep)(using Interactive.contextOfPath(path))
  }

  def computeCompletions(
    pos: SourcePosition,
    path: List[Tree],
    dependencyCompleteOpt: Option[String => (Int, Seq[String])],
    enableDeep: Boolean
  )(using Context): (Int, List[Completion]) = {
    val mode = Completion.completionMode(path, pos)
    val prefix = Completion.completionPrefix(path, pos)
    val completer = new DeepCompleter(mode, prefix, pos)

    var extra = List.empty[Completion]

    val completions = path match {
      case (sel : Select) :: _                                 => completer.selectionCompletions(sel)
      case Import(Ident(name), _) :: _ if name.decode.toString == "$ivy" && dependencyCompleteOpt.nonEmpty =>
        val complete = dependencyCompleteOpt.get
        val (pos, completions) = complete(prefix)
        val input0 = prefix.take(pos)
        extra ++= completions.distinct.toList
          .map(s => Completion(label(termName(input0 + s)), "", Nil))
        Map.empty
      case Import(expr, _) :: _                              => completer.directMemberCompletions(expr)
      case (_: untpd.ImportSelector) :: Import(expr, _) :: _ => completer.directMemberCompletions(expr)
      case _                                                 =>
        completer.scopeCompletions ++ {
          if (enableDeep) completer.deepCompletions
          else Nil
        }
    }

    val describedCompletions = extra ++ describeCompletions(completions)
    val offset = Completion.completionOffset(path)

    (pos.span.start - prefix.length, describedCompletions)
  }

  private type CompletionMap = Map[Name, Seq[SingleDenotation]]

  private def describeCompletions(completions: CompletionMap)(using Context): List[Completion] = {
    for
      (name, denots) <- completions.toList
      denot <- denots
    yield
      Completion(label(name), Completion.description(denot), List(denot.symbol))
  }

  class DeepCompleter(mode: Completion.Mode, prefix: String, pos: SourcePosition) extends Completion.Completer(mode, prefix, pos):
    def deepCompletions(using Context): Map[Name, Seq[SingleDenotation]] = {

      def allMembers(s: Symbol) =
        try s.info.allMembers
        catch {
          case _: dotty.tools.dotc.core.TypeError => Nil
        }
      def rec(t: Symbol): Seq[Symbol] = {
        val children =
          if (t.is(Package) || t.is(PackageVal) || t.is(PackageClass)) {
            allMembers(t).map(_.symbol).filter(_ != t).flatMap(rec)
          } else Nil

        t +: children.toSeq
      }

      val syms = for {
        member <- allMembers(defn.RootClass).map(_.symbol).toList
        sym <- rec(member)
        if sym.name.toString.startsWith(prefix)
      } yield sym

      syms.map(sym => (sym.fullName, List(sym: SingleDenotation))).toMap
    }

  private val bslash = '\\'
  private val specialChars = Set('[', ']', '(', ')', '{', '}', '.', ',', ';')

  def label(name: Name): String = {

    def maybeQuote(name: Name, recurse: Boolean): String =
      if (recurse && name.isTermName)
        name.asTermName.qualToString(maybeQuote(_, true), maybeQuote(_, false))
      else {
        // initially adapted from
        // https://github.com/scala/scala/blob/decbd53f1bde4600c8ff860f30a79f028a8e431d/src/reflect/scala/reflect/internal/Printers.scala#L573-L584
        val decName = name.decode.toString
        val hasSpecialChar = decName.exists { ch =>
          specialChars(ch) || ch.isWhitespace
        }
        def isOperatorLike = (name.isOperatorName || decName.exists(isOperatorPart)) &&
          decName.exists(isScalaLetter) &&
          !decName.contains(bslash)
        lazy val term = name.toTermName

        val needsBackTicks = hasSpecialChar ||
          isOperatorLike ||
          nme.keywords(term) && term != nme.USCOREkw

        if (needsBackTicks) s"`$decName`"
        else decName
      }

    maybeQuote(name, true)
  }
}

