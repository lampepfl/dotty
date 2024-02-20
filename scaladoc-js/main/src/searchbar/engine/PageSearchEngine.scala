package dotty.tools.scaladoc

import scala.concurrent.{ Future, ExecutionContext }
import concurrent.ExecutionContext.Implicits.global
import math.Ordering.Implicits.seqOrdering
import org.scalajs.dom.Node

import scala.annotation.tailrec

/**
 * TODO:
 * - Prematcher simple scoring
 * - Test first token score
 * - Maybe matcher for len > 1?
 * - Fix kinds (class List)
 * - Search for docs in Docs, for classes/etc. in Api.
 * - Write tests! Lists of pages and assert ordering.
 * - Optimize.
 */
class PageSearchEngine(pages: List[PageEntry]):

  def query(query: NameAndKindQuery): List[MatchResult] = {
    matchPages(query)
      .filter {
        case MatchResult(score, _, _) => score >= 0
      }
      .sortBy {
        case MatchResult(score, _, _) => -score
      }
  }

  private def kindScoreBonus(kind: String): Int = kind match {
    case "class"                          => 5
    case "object" | "enum"                => 4
    case "trait"                          => 3
    case "def" | "val" | "given" | "type" => 1
    case _                                => 0
  }

  private val positionScores = List(8,4,2,1).orElse(PartialFunction.fromFunction(_ => 0))

  private def matchCompletnessBonus(nameCharacters: Int, matchCharacters: Int): Int =
    (matchCharacters * 6) / nameCharacters +
      (if nameCharacters == matchCharacters then 2 else 0)

  private def matchPages(query: NameAndKindQuery): List[MatchResult] = query match
    case NameAndKindQuery(None, None) => List.empty
    case NameAndKindQuery(None, Some(kind)) =>
      filterKind(pages, kind)
        .map(MatchResult(1, _, Set.empty))
    case NameAndKindQuery(Some(""), kind) =>
      kind.fold(pages)(filterKind(pages, _))
        .map(MatchResult(1, _, Set.empty))
    case NameAndKindQuery(Some(nameSearch), kind) =>
      val kindFiltered = kind.fold(pages)(filterKind(pages, _))
      val prematchedPages = prematchPages(kindFiltered, nameSearch)

      if nameSearch.length > 1 then
        prematchedPages.map { prematched =>
          val finalMatch = matchPage(prematched, nameSearch)
          val bonusScore = kindScoreBonus(prematched.pageEntry.kind)
            + matchCompletnessBonus(prematched.pageEntry.shortName.length, nameSearch.length)
          finalMatch.copy(score = finalMatch.score + bonusScore)
        }
      else prematchedPages

  private def filterKind(pages: List[PageEntry], kind: String): List[PageEntry] =
    pages.filter(_.kind == kind)

  def prematchPages(pages: List[PageEntry], search: String): List[MatchResult] =
    pages.map(prematchPage(_, search)).filter(_.indices.nonEmpty)

  private def prematchPage(page: PageEntry, search: String): MatchResult =
    val pagePackage = page.description
    val pageName = page.shortName
    val fullQualified = if page.description != "" then
      page.description + "." + pageName
      else pageName

    @tailrec
    def prematchPageAcc(nameIndex: Int, searchIndex: Int, fullIndex: Int, acc: Set[Int], scoreAcc: Int, consecutiveMatches: Int): MatchResult =
      if searchIndex >= search.length then
        MatchResult(scoreAcc, page, acc)
      else if nameIndex >= pageName.length || fullIndex >= fullQualified.length then
          MatchResult(0, page, Set.empty)
      else if search.contains(".") && fullQualified(fullIndex).toLower == search(searchIndex).toLower then
        val score = (if consecutiveMatches > 0 then 1 else 0) + positionScores(nameIndex)
        prematchPageAcc(nameIndex, searchIndex + 1, fullIndex + 1, acc + nameIndex, scoreAcc + score, consecutiveMatches + 1)
      else if pageName(nameIndex).toLower == search(searchIndex).toLower then
        val score = (if consecutiveMatches > 0 then 1 else 0) + positionScores(nameIndex)
        prematchPageAcc(nameIndex + 1, searchIndex + 1, fullIndex, acc + nameIndex, scoreAcc + score, consecutiveMatches + 1)
      else
        prematchPageAcc(nameIndex + 1, searchIndex, fullIndex + 1, acc, scoreAcc, 0)

    val result = prematchPageAcc(0, 0, 0, Set.empty, 0, 0)
    result.copy(score = result.score + kindScoreBonus(page.kind))

  private def matchPage(prematched: MatchResult, nameSearch: String): MatchResult =
    val searchTokens: List[List[Char]] = StringUtils.createCamelCaseTokens(nameSearch).map(_.toList) //todo extract
    val pageTokens: List[List[Char]] = prematched.pageEntry.tokens.map(_.toList)
    val pageName = prematched.pageEntry.shortName
    val searchTokensLifted = searchTokens.lift
    val pageTokensLifted = pageTokens.lift

    @tailrec
    def matchTokens(searchTokenIndex: Int, pageTokenIndex: Int, acc: Set[(Int, Int)]): Set[(Int, Int)] =
      (searchTokensLifted(searchTokenIndex).map(_.toList), pageTokensLifted(pageTokenIndex).map(_.toList)) match
        case (None, _) | (_, None) => acc
        case (Some(searchHead :: _), Some(pageHead :: _)) =>
          if searchHead == pageHead then
            matchTokens(searchTokenIndex + 1, pageTokenIndex + 1, acc + ((searchTokenIndex, pageTokenIndex)))
          else
            matchTokens(searchTokenIndex, pageTokenIndex + 1, acc)
        // empty tokens edge cases
        case (Some(_), Some(_ :: _)) => matchTokens(searchTokenIndex + 1, pageTokenIndex, acc)
        case (Some(_ :: _), Some(_)) => matchTokens(searchTokenIndex, pageTokenIndex + 1, acc)
        case _                       => matchTokens(searchTokenIndex + 1, pageTokenIndex + 1, acc)
    end matchTokens

    val matchedTokens = matchTokens(0, 0, Set.empty)
    val searchTokenPositions = searchTokens.map(_.length).scanLeft(0)(_ + _)
    val pageTokensPositions = pageTokens.map(_.length).scanLeft(0)(_ + _)

    @tailrec
    def findHighScoreMatch(
        searchTokenIndex: Int,
        searchPosition: Int,
        pageTokenIndex: Int,
        pagePosition: Int,
        positionAcc: Set[Int],
        scoreAcc: Int,
        consecutiveMatches: Int
      ): Option[MatchResult] =
      if searchPosition >= nameSearch.length then
          Some(MatchResult(scoreAcc, prematched.pageEntry, positionAcc))
      else if pagePosition >= pageName.length then
        None
      else
        val currentSearchTokenStart = searchTokenPositions(searchTokenIndex)
        val matchingPageToken = matchedTokens.find(_._1 == searchTokenIndex).map(_._2)
        val searchChar = nameSearch.charAt(searchPosition).toLower
        val pageChar = pageName.charAt(pagePosition).toLower

        def recalculateTokenIndex(tokenPositions: Seq[Int], previousIndex: Int, position: Int): Int =
          if tokenPositions.length <= previousIndex + 1 || tokenPositions(previousIndex + 1) > position then
            previousIndex
          else
            previousIndex + 1

        def getMatchScore(matchedPagePosition: Int, matchedPageTokenStart: Int): Int =
          val consecutiveMatchesScore = if consecutiveMatches > 0 then 1 else 0
          val matchPositionScore = positionScores(matchedPagePosition - matchedPageTokenStart)
          val firstTokenScore = if matchPositionScore > 0 && matchedPageTokenStart == 0 then 3 else 0
          consecutiveMatchesScore + matchPositionScore + firstTokenScore

        matchingPageToken match
          case Some(matchingToken) if searchPosition == currentSearchTokenStart =>
            val matchedTokenPosition = pageTokensPositions(matchingToken)
            findHighScoreMatch(
              recalculateTokenIndex(searchTokenPositions, searchTokenIndex, searchPosition + 1),
              searchPosition + 1,
              recalculateTokenIndex(pageTokensPositions, pageTokenIndex, matchedTokenPosition + 1),
              matchedTokenPosition + 1,
              positionAcc + matchedTokenPosition,
              scoreAcc + getMatchScore(matchedTokenPosition, matchedTokenPosition),
              consecutiveMatches + 1
            )
          case _  if searchChar == pageChar =>
            val matchedTokenPosition = matchingPageToken.map(pageTokensPositions).getOrElse(0)
            findHighScoreMatch(
              recalculateTokenIndex(searchTokenPositions, searchTokenIndex, searchPosition + 1),
              searchPosition + 1,
              recalculateTokenIndex(pageTokensPositions, pageTokenIndex, pagePosition + 1),
              pagePosition + 1,
              positionAcc + pagePosition,
              scoreAcc + getMatchScore(pagePosition, matchedTokenPosition),
              consecutiveMatches + 1
            )
          case _ =>
            findHighScoreMatch(
              searchTokenIndex,
              searchPosition,
              recalculateTokenIndex(pageTokensPositions, pageTokenIndex, pagePosition + 1),
              pagePosition + 1,
              positionAcc,
              scoreAcc,
              0
            )

    val highScoreMatch = findHighScoreMatch(0, 0, 0, 0, Set.empty, 0, 0)
    highScoreMatch.getOrElse(prematched)
