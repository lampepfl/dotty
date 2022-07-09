package dotty.tools
package dotc
package parsing

import scala.language.unsafeNulls

import collection.immutable.BitSet
import core.Decorators._
import core.StdNames.nme

abstract class TokensCommon {
  def maxToken: Int

  type Token = Int
  type TokenSet = BitSet

  def tokenRange(lo: Int, hi: Int): TokenSet = BitSet(lo to hi: _*)

  val tokenString, debugString: Array[String] = new Array[String](maxToken + 1)

  def enter(token: Int, str: String, debugStr: String = ""): Unit = {
    assert(tokenString(token) == null)
    tokenString(token) = str
    debugString(token) = if (debugStr.isEmpty) str else debugStr
  }

  /** special tokens */
  inline val EMPTY = 0;             enter(EMPTY, "<empty>") // a missing token, used in lookahead
  inline val ERROR = 1;             enter(ERROR, "erroneous token") // an erroneous token
  inline val EOF = 2;               enter(EOF, "eof")

  /** literals */
  inline val CHARLIT = 3;           enter(CHARLIT, "character literal")
  inline val INTLIT = 4;            enter(INTLIT, "integer literal")
  inline val DECILIT = 5;           enter(DECILIT, "number literal")  // with decimal point
  inline val EXPOLIT = 6;           enter(EXPOLIT, "number literal with exponent")
  inline val LONGLIT = 7;           enter(LONGLIT, "long literal")
  inline val FLOATLIT = 8;          enter(FLOATLIT, "float literal")
  inline val DOUBLELIT = 9;         enter(DOUBLELIT, "double literal")
  inline val STRINGLIT = 10;         enter(STRINGLIT, "string literal")
  inline val STRINGPART = 11;       enter(STRINGPART, "string literal", "string literal part")
  //inline val INTERPOLATIONID = 12;  enter(INTERPOLATIONID, "string interpolator")
  //inline val QUOTEID = 13;        enter(QUOTEID, "quoted identifier") // TODO: deprecate

  /** identifiers */
  inline val IDENTIFIER = 14;       enter(IDENTIFIER, "identifier")
  //inline val BACKQUOTED_IDENT = 15; enter(BACKQUOTED_IDENT, "identifier", "backquoted ident")

  /** alphabetic keywords */
  inline val IF = 20;               enter(IF, "if")
  inline val FOR = 21;              enter(FOR, "for")
  inline val ELSE = 22;             enter(ELSE, "else")
  inline val THIS = 23;             enter(THIS, "this")
  inline val NULL = 24;             enter(NULL, "null")
  inline val NEW = 25;              enter(NEW, "new")
  //inline val WITH = 26;             enter(WITH, "with")
  inline val SUPER = 27;            enter(SUPER, "super")
  //inline val CASE = 28;             enter(CASE, "case")
  //inline val CASECLASS = 29;        enter(CASECLASS, "case class")
  //inline val CASEOBJECT = 30;       enter(CASEOBJECT, "case object")
  //inline val VAL = 31;              enter(VAL, "val")
  inline val ABSTRACT = 32;         enter(ABSTRACT, "abstract")
  inline val FINAL = 33;            enter(FINAL, "final")
  inline val PRIVATE = 34;          enter(PRIVATE, "private")
  inline val PROTECTED = 35;        enter(PROTECTED, "protected")
  inline val OVERRIDE = 36;         enter(OVERRIDE, "override")
  //inline val IMPLICIT = 37;         enter(IMPLICIT, "implicit")
  //inline val VAR = 38;              enter(VAR, "var")
  //inline val DEF = 39;              enter(DEF, "def")
  //inline val TYPE = 40;             enter(TYPE, "type")
  inline val EXTENDS = 41;          enter(EXTENDS, "extends")
  inline val TRUE = 42;             enter(TRUE, "true")
  inline val FALSE = 43;            enter(FALSE, "false")
  //inline val OBJECT = 44;           enter(OBJECT, "object")
  inline val CLASS = 45;            enter(CLASS, "class")
  inline val IMPORT = 46;           enter(IMPORT, "import")
  inline val PACKAGE = 47;          enter(PACKAGE, "package")
  //inline val YIELD = 48;            enter(YIELD, "yield")
  inline val DO = 49;               enter(DO, "do")
  //inline val TRAIT = 50;            enter(TRAIT, "trait")
  //inline val SEALED = 51;           enter(SEALED, "sealed")
  inline val THROW = 52;            enter(THROW, "throw")
  inline val TRY = 53;              enter(TRY, "try")
  inline val CATCH = 54;            enter(CATCH, "catch")
  inline val FINALLY = 55;          enter(FINALLY, "finally")
  inline val WHILE = 56;            enter(WHILE, "while")
  inline val RETURN = 57;           enter(RETURN, "return")
  //inline val MATCH = 58;            enter(MATCH, "match")
  //inline val LAZY = 59;             enter(LAZY, "lazy")
  //inline val THEN = 60;             enter(THEN, "then")
  //inline val FORSOME = 61;          enter(FORSOME, "forSome") // TODO: deprecate
  //inline val ENUM = 62;             enter(ENUM, "enum")

  /** special symbols */
  inline val COMMA = 70;            enter(COMMA, "','")
  inline val SEMI = 71;             enter(SEMI, "';'")
  inline val DOT = 72;              enter(DOT, "'.'")
  //inline val NEWLINE = 78;          enter(NEWLINE, "end of statement", "new line")
  //inline val NEWLINES = 79;         enter(NEWLINES, "end of statement", "new lines")

  /** special keywords */
  //inline val USCORE = 73;           enter(USCORE, "_")
  inline val COLONop = 74;          enter(COLONop, ":")  // a stand-alone `:`, see also COLONfollow
  inline val EQUALS = 75;           enter(EQUALS, "=")
  //inline val LARROW = 76;           enter(LARROW, "<-")
  //inline val ARROW = 77;            enter(ARROW, "=>")
  //inline val SUBTYPE = 80;          enter(SUBTYPE, "<:")
  //inline val SUPERTYPE = 81;        enter(SUPERTYPE, ">:")
  //inline val HASH = 82;             enter(HASH, "#")
  inline val AT = 83;               enter(AT, "@")
  //inline val VIEWBOUND = 84;        enter(VIEWBOUND, "<%")

  val keywords: TokenSet

  def isKeyword(token: Token): Boolean = keywords contains token

  /** parentheses */
  inline val LPAREN = 91;           enter(LPAREN, "'('")
  inline val RPAREN = 92;           enter(RPAREN, "')'")
  inline val LBRACKET = 93;         enter(LBRACKET, "'['")
  inline val RBRACKET = 94;         enter(RBRACKET, "']'")
  inline val LBRACE = 95;           enter(LBRACE, "'{'")
  inline val RBRACE = 96;           enter(RBRACE, "'}'")
  inline val INDENT = 97;           enter(INDENT, "indent")
  inline val OUTDENT = 98;          enter(OUTDENT, "unindent")

  inline val firstParen = LPAREN
  inline val lastParen = OUTDENT

  def buildKeywordArray(keywords: TokenSet): (Int, Array[Int]) = {
    def start(tok: Token) = tokenString(tok).toTermName.asSimpleName.start
    def sourceKeywords = keywords.toList.filter { (kw: Token) =>
      val ts = tokenString(kw)
      (ts != null) && !ts.contains(' ')
    }

    val lastKeywordStart = sourceKeywords.map(start).max

    val arr = Array.fill(lastKeywordStart + 1)(IDENTIFIER)
    for (kw <- sourceKeywords) arr(start(kw)) = kw
    (lastKeywordStart, arr)
  }
}

object Tokens extends TokensCommon {
  inline val minToken = EMPTY
  final def maxToken: Int = XMLSTART

  inline val INTERPOLATIONID = 12;  enter(INTERPOLATIONID, "string interpolator")
  inline val QUOTEID = 13;          enter(QUOTEID, "quoted identifier") // TODO: deprecate

  inline val BACKQUOTED_IDENT = 15; enter(BACKQUOTED_IDENT, "identifier", "backquoted ident")

  final val identifierTokens: TokenSet = BitSet(IDENTIFIER, BACKQUOTED_IDENT)

  def isIdentifier(token : Int): Boolean =
    token >= IDENTIFIER && token <= BACKQUOTED_IDENT

  /** alphabetic keywords */
  inline val WITH = 26;             enter(WITH, "with")
  inline val CASE = 28;             enter(CASE, "case")
  inline val CASECLASS = 29;        enter(CASECLASS, "case class")
  inline val CASEOBJECT = 30;       enter(CASEOBJECT, "case object")
  inline val VAL = 31;              enter(VAL, "val")
  inline val IMPLICIT = 37;         enter(IMPLICIT, "implicit")
  inline val VAR = 38;              enter(VAR, "var")
  inline val DEF = 39;              enter(DEF, "def")
  inline val TYPE = 40;             enter(TYPE, "type")
  inline val OBJECT = 44;           enter(OBJECT, "object")
  inline val YIELD = 48;            enter(YIELD, "yield")
  inline val TRAIT = 50;            enter(TRAIT, "trait")
  inline val SEALED = 51;           enter(SEALED, "sealed")
  inline val MATCH = 58;            enter(MATCH, "match")
  inline val LAZY = 59;             enter(LAZY, "lazy")
  inline val THEN = 60;             enter(THEN, "then")
  inline val FORSOME = 61;          enter(FORSOME, "forSome") // TODO: deprecate
  inline val ENUM = 62;             enter(ENUM, "enum")
  inline val GIVEN = 63;            enter(GIVEN, "given")
  inline val EXPORT = 64;           enter(EXPORT, "export")
  inline val MACRO = 65;            enter(MACRO, "macro") // TODO: remove
  inline val END = 66;              enter(END, "end")

  /** special symbols */
  inline val NEWLINE = 78;          enter(NEWLINE, "end of statement", "new line")
  inline val NEWLINES = 79;         enter(NEWLINES, "end of statement", "new lines")

  /** special keywords */
  inline val USCORE = 73;           enter(USCORE, "_")
  inline val LARROW = 76;           enter(LARROW, "<-")
  inline val ARROW = 77;            enter(ARROW, "=>")
  inline val SUBTYPE = 80;          enter(SUBTYPE, "<:")
  inline val SUPERTYPE = 81;        enter(SUPERTYPE, ">:")
  inline val HASH = 82;             enter(HASH, "#")
  inline val VIEWBOUND = 84;        enter(VIEWBOUND, "<%")
  inline val TLARROW = 85;          enter(TLARROW, "=>>")
  inline val CTXARROW = 86;         enter(CTXARROW, "?=>")

  inline val QUOTE = 87;            enter(QUOTE, "'")

  inline val COLONfollow = 88;      enter(COLONfollow, ":")
    // A `:` following an alphanumeric identifier or one of the tokens in colonEOLPredecessors
  inline val COLONeol = 89;         enter(COLONeol, ":", ": at eol")
    // A `:` recognized as starting an indentation block
  inline val SELFARROW = 90;        enter(SELFARROW, "=>") // reclassified ARROW following self-type

  /** XML mode */
  inline val XMLSTART = 99;         enter(XMLSTART, "$XMLSTART$<") // TODO: deprecate

  final val alphaKeywords: TokenSet = tokenRange(IF, END)
  final val symbolicKeywords: TokenSet = tokenRange(USCORE, CTXARROW)
  final val keywords: TokenSet = alphaKeywords | symbolicKeywords

  final val allTokens: TokenSet = tokenRange(minToken, maxToken)

  final val simpleLiteralTokens: TokenSet =
    tokenRange(CHARLIT, STRINGLIT) | BitSet(TRUE, FALSE)
  final val literalTokens: TokenSet = simpleLiteralTokens | BitSet(INTERPOLATIONID, QUOTEID, NULL)  // TODO: drop QUOTEID when symbol literals are gone

  final val atomicExprTokens: TokenSet = literalTokens | identifierTokens | BitSet(
    USCORE, NULL, THIS, SUPER, TRUE, FALSE, RETURN, QUOTEID, XMLSTART)

  final val openParensTokens = BitSet(LBRACE, LPAREN, LBRACKET)

  final val canStartExprTokens3: TokenSet =
      atomicExprTokens
    | openParensTokens
    | BitSet(INDENT, QUOTE, IF, WHILE, FOR, NEW, TRY, THROW)

  final val canStartExprTokens2: TokenSet = canStartExprTokens3 | BitSet(DO)

  final val canStartTypeTokens: TokenSet = literalTokens | identifierTokens | BitSet(
    THIS, SUPER, USCORE, LPAREN, LBRACE, AT)

  final val templateIntroTokens: TokenSet = BitSet(CLASS, TRAIT, OBJECT, ENUM, CASECLASS, CASEOBJECT)

  final val dclIntroTokens: TokenSet = BitSet(DEF, VAL, VAR, TYPE, GIVEN)

  final val defIntroTokens: TokenSet = templateIntroTokens | dclIntroTokens

  final val localModifierTokens: TokenSet = BitSet(ABSTRACT, FINAL, SEALED, IMPLICIT, LAZY)

  final val accessModifierTokens: TokenSet = BitSet(
    PRIVATE, PROTECTED)

  final val modifierTokens: TokenSet = localModifierTokens | accessModifierTokens | BitSet(
    OVERRIDE)

  final val modifierTokensOrCase: TokenSet = modifierTokens | BitSet(CASE)

  final val modifierFollowers = modifierTokensOrCase | defIntroTokens

  /** Is token only legal as start of statement (eof also included)? */
  final val mustStartStatTokens: TokenSet = defIntroTokens | modifierTokens | BitSet(IMPORT, EXPORT, PACKAGE)

  final val canStartStatTokens2: TokenSet = canStartExprTokens2 | mustStartStatTokens | BitSet(
    AT, CASE, END) // END is included since it might be tested before being converted back to IDENTIFIER
  final val canStartStatTokens3: TokenSet = canStartExprTokens3 | mustStartStatTokens | BitSet(
    AT, CASE, END)

  final val canEndStatTokens: TokenSet = atomicExprTokens | BitSet(TYPE, GIVEN, RPAREN, RBRACE, RBRACKET, OUTDENT)

  /** Tokens that stop a lookahead scan search for a `<-`, `then`, or `do`.
   *  Used for disambiguating between old and new syntax.
   */
  final val stopScanTokens: BitSet = mustStartStatTokens |
    BitSet(IF, ELSE, WHILE, DO, FOR, YIELD, NEW, TRY, CATCH, FINALLY, THROW, RETURN, MATCH, SEMI, EOF)

  final val numericLitTokens: TokenSet = BitSet(INTLIT, DECILIT, EXPOLIT, LONGLIT, FLOATLIT, DOUBLELIT)

  final val statCtdTokens: BitSet = BitSet(THEN, ELSE, DO, CATCH, FINALLY, YIELD, MATCH)

  final val closingRegionTokens = BitSet(RBRACE, RPAREN, RBRACKET, CASE) | statCtdTokens

  final val canStartIndentTokens: BitSet =
    statCtdTokens | BitSet(COLONeol, WITH, EQUALS, ARROW, CTXARROW, LARROW, WHILE, TRY, FOR, IF, THROW, RETURN)

  /** Faced with the choice between a type and a formal parameter, the following
   *  tokens determine it's a formal parameter.
   */
  final val startParamTokens: BitSet = modifierTokens | BitSet(VAL, VAR, AT)

  final val scala3keywords = BitSet(ENUM, GIVEN)

  final val endMarkerTokens = identifierTokens | BitSet(IF, WHILE, FOR, MATCH, TRY, NEW, THROW, GIVEN, VAL, THIS)

  final val colonEOLPredecessors = BitSet(RPAREN, RBRACKET, BACKQUOTED_IDENT, THIS, SUPER, NEW)

  final val closingParens = BitSet(RPAREN, RBRACKET, RBRACE)

  final val softModifierNames = Set(nme.inline, nme.opaque, nme.open, nme.transparent, nme.infix)

  def showTokenDetailed(token: Int): String = debugString(token)

  def showToken(token: Int): String = {
    val str = tokenString(token)
    if isKeyword(token) || token == COLONfollow || token == COLONeol then s"'$str'" else str
  }
}
