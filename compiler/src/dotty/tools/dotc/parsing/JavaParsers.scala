package dotty.tools
package dotc
package parsing

import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.FlagSet

import JavaTokens.*
import JavaScanners.*
import Scanners.Offset
import Parsers.*
import core.*
import Contexts.*
import Symbols.defn
import Names.*
import Types.*
import ast.Trees.*
import Decorators.*
import StdNames.*
import reporting.*
import dotty.tools.dotc.util.SourceFile
import util.Spans.*

import scala.collection.mutable.{ListBuffer, LinkedHashMap}

object JavaParsers {

  import ast.untpd.*


  val fakeFlags = Flags.JavaDefined | Flags.PrivateLocal | Flags.Invisible

  class JavaParser(source: SourceFile)(using Context) extends ParserCommon(source) {

    val definitions: Definitions = ctx.definitions
    import definitions.*

    val in: JavaScanner = new JavaScanner(source)

    /** The simple name of the package of the currently parsed file */
    private var thisPackageName: TypeName = tpnme.EMPTY

    /** This is the general parse entry point.
     *  Overridden by ScriptParser
     */
    def parse(): Tree = {
      val t = compilationUnit()
      accept(EOF)
      t
    }

    // -------- error handling ---------------------------------------

    protected def skip(): Unit = {
      var nparens = 0
      var nbraces = 0
      while (true) {
        in.token match {
          case EOF =>
            return
          case SEMI =>
            if (nparens == 0 && nbraces == 0) return
          case RPAREN =>
            nparens -= 1
          case RBRACE =>
            if (nbraces == 0) return
            nbraces -= 1
          case LPAREN =>
            nparens += 1
          case LBRACE =>
            nbraces += 1
          case _ =>
        }
        in.nextToken()
      }
    }

    def syntaxError(msg: Message, skipIt: Boolean): Unit =
      syntaxError(in.offset, msg, skipIt)

    def syntaxError(offset: Int, msg: Message, skipIt: Boolean): Unit = {
      if (offset > lastErrorOffset) {
        syntaxError(msg, offset)
        // no more errors on this token.
        lastErrorOffset = in.offset
      }
      if (skipIt)
        skip()
    }

    def errorTypeTree: TypeTree = TypeTree().withType(UnspecifiedErrorType).withSpan(Span(in.offset))

    // --------- tree building -----------------------------

    def javaDot(name: Name): Tree =
      Select(rootDot(nme.java), name)

    def javaLangDot(name: Name): Tree =
      Select(javaDot(nme.lang), name)

    /** Synthetic tree representing `java.lang.Object`.
     *  The typer will type all references to `java.lang.Object` as `FromJavaObject`.
     */
    def ObjectTpt(): Tree = TypeTree(defn.FromJavaObjectType) // javaLangDot(tpnme.Object)

    /** Tree representing `java.lang.Record` */
    def javaLangRecord(): Tree = javaLangDot(tpnme.Record)

    def arrayOf(tpt: Tree): AppliedTypeTree =
      AppliedTypeTree(scalaDot(tpnme.Array), List(tpt))

    def makeTemplate(parents: List[Tree], stats: List[Tree], tparams: List[TypeDef], needsDummyConstr: Boolean): Template = {
      def UnitTpt(): Tree = TypeTree(defn.UnitType)

      def pullOutFirstConstr(stats: List[Tree]): (Tree, List[Tree]) = stats match {
        case (meth: DefDef) :: rest if meth.name == nme.CONSTRUCTOR => (meth, rest)
        case first :: rest =>
          val (constr, tail) = pullOutFirstConstr(rest)
          (constr, first :: tail)
        case nil => (EmptyTree, nil)
      }
      var (constr1, stats1) = pullOutFirstConstr(stats)
      // A dummy first constructor is needed for Java classes so that the real constructors see the
      // import of the companion object. The constructor has parameter of type Unit so no Java code
      // can call it.
      // This also avoids clashes between the constructor parameter names and member names.
      if (needsDummyConstr) {
        if (constr1 == EmptyTree) constr1 = makeConstructor(List(), Nil, Parsers.unimplementedExpr)
        stats1 = constr1 :: stats1
        constr1 = makeConstructor(List(UnitTpt()), tparams, EmptyTree, fakeFlags)
      }
      else if (constr1 == EmptyTree) {
        constr1 = makeConstructor(List(), tparams, EmptyTree)
      }
      Template(constr1.asInstanceOf[DefDef], parents, Nil, EmptyValDef, stats1)
    }

    def makeSyntheticParam(count: Int, tpt: Tree): ValDef =
      makeParam(nme.syntheticParamName(count), tpt)
    def makeParam(name: TermName, tpt: Tree): ValDef =
      ValDef(name, tpt, EmptyTree).withFlags(Flags.JavaDefined | Flags.Param)

    def makeConstructor(formals: List[Tree], tparams: List[TypeDef], body: Tree, flags: FlagSet = Flags.JavaDefined): DefDef = {
      val vparams = formals.zipWithIndex.map { case (p, i) => makeSyntheticParam(i + 1, p).withAddedFlags(flags) }
      DefDef(nme.CONSTRUCTOR, joinParams(tparams, List(vparams)), TypeTree(), body).withFlags(flags)
    }

    // ------------- general parsing ---------------------------

    /** skip parent or brace enclosed sequence of things */
    def skipAhead(): Unit = {
      var nparens = 0
      var nbraces = 0
      while ({
        in.token match {
          case LPAREN =>
            nparens += 1
          case LBRACE =>
            nbraces += 1
          case _ =>
        }
        in.nextToken()
        in.token match {
          case RPAREN =>
            nparens -= 1
          case RBRACE =>
            nbraces -= 1
          case _ =>
        }
        in.token != EOF && (nparens > 0 || nbraces > 0)
      })
      ()
    }

    def skipTo(tokens: Int*): Unit =
      while (!(tokens contains in.token) && in.token != EOF)
        if (in.token == LBRACE) { skipAhead(); accept(RBRACE) }
        else if (in.token == LPAREN) { skipAhead(); accept(RPAREN) }
        else in.nextToken()

    /** Consume one token of the specified type, or
      * signal an error if it is not there.
      *
      * @return The offset at the start of the token to accept
      */
    def accept(token: Int): Int = {
      val offset = in.offset
      if (in.token != token) {
        val offsetToReport = in.offset
        val msg =
          em"${tokenString(token)} expected but ${tokenString(in.token)} found."
        syntaxError(offsetToReport, msg, skipIt = true)
      }
      if (in.token == token) in.nextToken()
      offset
    }

    def acceptClosingAngle(): Unit = {
      val closers: PartialFunction[Int, Int] = {
        case GTGTGTEQ => GTGTEQ
        case GTGTGT   => GTGT
        case GTGTEQ   => GTEQ
        case GTGT     => GT
        case GTEQ     => EQUALS
      }
      if (closers isDefinedAt in.token) in.token = closers(in.token)
      else accept(GT)
    }

    def identForType(): TypeName = ident().toTypeName
    def ident(): Name =
      if (in.token == IDENTIFIER) {
        val name = in.name
        in.nextToken()
        name
      }
      else {
        accept(IDENTIFIER)
        nme.ERROR
      }

    def repsep[T <: Tree](p: () => T, sep: Int): List[T] = {
      val buf = ListBuffer[T](p())
      while (in.token == sep) {
        in.nextToken()
        buf += p()
      }
      buf.toList
    }

    /** Convert (qual)ident to type identifier
      */
    def convertToTypeId(tree: Tree): Tree = convertToTypeName(tree) match {
      case Some(t)  => t.withSpan(tree.span)
      case _        => tree match {
        case AppliedTypeTree(_, _) | Select(_, _) =>
          tree
        case _ =>
          syntaxError(IdentifierExpected(tree.show), tree.span)
          errorTypeTree
      }
    }

    /** Translate names in Select/Ident nodes to type names.
      */
    def convertToTypeName(tree: Tree): Option[RefTree] = tree match {
      case Select(qual, name) => Some(Select(qual, name.toTypeName))
      case Ident(name)        => Some(Ident(name.toTypeName))
      case _                  => None
    }
    // -------------------- specific parsing routines ------------------

    def qualId(): RefTree = {
      var t: RefTree = atSpan(in.offset) { Ident(ident()) }
      while (in.token == DOT && in.lookaheadToken == IDENTIFIER) {
        in.nextToken()
        t = atSpan(t.span.start, in.offset) { Select(t, ident()) }
      }
      t
    }

    def optArrayBrackets(tpt: Tree): Tree =
      if (in.token == LBRACKET) {
        val tpt1 = atSpan(tpt.span.start, in.offset) { arrayOf(tpt) }
        in.nextToken()
        accept(RBRACKET)
        optArrayBrackets(tpt1)
      }
      else tpt

    def basicType(): Tree =
      atSpan(in.offset) {
        in.token match {
          case BYTE    => in.nextToken(); TypeTree(ByteType)
          case SHORT   => in.nextToken(); TypeTree(ShortType)
          case CHAR    => in.nextToken(); TypeTree(CharType)
          case INT     => in.nextToken(); TypeTree(IntType)
          case LONG    => in.nextToken(); TypeTree(LongType)
          case FLOAT   => in.nextToken(); TypeTree(FloatType)
          case DOUBLE  => in.nextToken(); TypeTree(DoubleType)
          case BOOLEAN => in.nextToken(); TypeTree(BooleanType)
          case _       => syntaxError(em"illegal start of type", skipIt = true); errorTypeTree
        }
      }

    def typ(): Tree =
      annotations()
      optArrayBrackets {
        if (in.token == FINAL) in.nextToken()
        if (in.token == IDENTIFIER) {
          var t = typeArgs(atSpan(in.offset)(Ident(ident())))
          // typeSelect generates Select nodes if the lhs is an Ident or Select,
          // For other nodes it always assumes that the selected item is a type.
          def typeSelect(t: Tree, name: Name) = t match {
            case Ident(_) | Select(_, _) => Select(t, name)
            case _ => Select(t, name.toTypeName)
          }
          while (in.token == DOT) {
            in.nextToken()
            t = typeArgs(atSpan(t.span.start, in.offset)(typeSelect(t, ident())))
          }
          convertToTypeId(t)
        }
        else
          basicType()
      }

    def typeArgs(t: Tree): Tree = {
      var wildnum = 0
      def typeArg(): Tree =
        if (in.token == QMARK) {
          val offset = in.offset
          in.nextToken()
          val hi = if (in.token == EXTENDS) { in.nextToken() ; typ() } else ObjectTpt()
          val lo = if (in.token == SUPER)   { in.nextToken() ; typ() } else EmptyTree
          atSpan(offset) {
            /*
            TypeDef(
              Modifiers(Flags.JavaDefined | Flags.Deferred),
              typeName("_$" +(wildnum += 1)),
              List(),
              TypeBoundsTree(lo, hi))
              */
            TypeBoundsTree(lo, hi)
          }
        }
        else
          typ()
      if (in.token == LT) {
        in.nextToken()
        val t1 = convertToTypeId(t)
        val args = repsep(() => typeArg(), COMMA)
        acceptClosingAngle()
        atSpan(t1.span.start) {
          AppliedTypeTree(t1, args)
        }
      }
      else t
    }

    def annotations(): List[Tree] = {
      var annots = new ListBuffer[Tree]
      while (in.token == AT) {
        in.nextToken()
        annotation() match {
          case Some(anno) => annots += anno
          case _ =>
        }
      }
      annots.toList
    }

    /** Annotation                   ::= TypeName [`(` [AnnotationArgument {`,` AnnotationArgument}] `)`]
      * AnnotationArgument           ::= ElementValuePair | ELementValue
      * ElementValuePair             ::= Identifier `=` ElementValue
      * ElementValue                 ::= ConstExpressionSubset
      *                                | ElementValueArrayInitializer
      *                                | Annotation
      * ElementValueArrayInitializer ::= `{` [ElementValue {`,` ElementValue}] [`,`]  `}`
      * ConstExpressionSubset        ::= Literal
      *                                | QualifiedName
      *                                | ClassLiteral
      *
      * We support only subset of const expressions expected in this context by java.
      * If we encounter expression that we cannot parse, we do not raise parsing error,
      * but instead we skip entire annotation silently.
      */
    def annotation(): Option[Tree] = {
      def classOrId(): Tree =
        val id = qualId()
        if in.lookaheadToken == CLASS then
          in.nextToken()
          accept(CLASS)
          TypeApply(
            Select(
              scalaDot(nme.Predef),
              nme.classOf),
            convertToTypeId(id) :: Nil
          )
        else id

      def array(): Option[Tree] =
        accept(LBRACE)
        val buffer = ListBuffer[Option[Tree]]()
        while in.token != RBRACE do
          buffer += argValue()
          if in.token == COMMA then
            in.nextToken() // using this instead of repsep allows us to handle trailing commas
        accept(RBRACE)
        Option.unless(buffer contains None) {
          Apply(scalaDot(nme.Array), buffer.flatten.toList)
        }

      def argValue(): Option[Tree] =
        val tree = tryConstant match {
          case Some(c) =>
            Some(atSpan(in.offset)(Literal(c)))
          case _ => in.token match {
            case AT =>
              in.nextToken()
              annotation()
            case IDENTIFIER => Some(classOrId())
            case LBRACE => array()
            case _ => None
          }
        }
        if in.token == COMMA || in.token == RBRACE || in.token == RPAREN then
          tree
        else
          skipTo(COMMA, RBRACE, RPAREN)
          None

      def annArg(): Option[Tree] =
        val name = if (in.token == IDENTIFIER && in.lookaheadToken == EQUALS)
          val n = ident()
          accept(EQUALS)
          n
        else
          nme.value
        argValue().map(NamedArg(name, _))


      val id = convertToTypeId(qualId())
      val args = ListBuffer[Option[Tree]]()
      if in.token == LPAREN then
        in.nextToken()
        if in.token != RPAREN then
          args += annArg()
          while in.token == COMMA do
            in.nextToken()
            args += annArg()
        accept(RPAREN)

      Option.unless(args contains None) {
        Apply(
          Select(New(id), nme.CONSTRUCTOR),
          args.flatten.toList
        )
      }
    }

    def modifiers(inInterface: Boolean): Modifiers = {
      var flags: FlagSet = Flags.JavaDefined
      // assumed true unless we see public/private/protected
      var isPackageAccess = true
      var annots = new ListBuffer[Tree]
      def addAnnot(tpt: Tree) =
        annots += atSpan(in.offset) {
          in.nextToken()
          New(tpt)
        }

      while (true)
        in.token match {
          case AT if (in.lookaheadToken != INTERFACE) =>
            in.nextToken()
            annotation() match {
              case Some(anno) => annots += anno
              case _ =>
            }
          case PUBLIC =>
            isPackageAccess = false
            in.nextToken()
          case PROTECTED =>
            flags |= Flags.Protected
            in.nextToken()
          case PRIVATE =>
            isPackageAccess = false
            flags |= Flags.Private
            in.nextToken()
          case STATIC =>
            flags |= Flags.JavaStatic
            in.nextToken()
          case ABSTRACT =>
            flags |= Flags.Abstract
            in.nextToken()
          case FINAL =>
            flags |= Flags.Final
            in.nextToken()
          case DEFAULT =>
            flags |= Flags.DefaultMethod
            in.nextToken()
          case NATIVE =>
            addAnnot(scalaDot(jtpnme.NATIVEkw))
          case TRANSIENT =>
            addAnnot(scalaDot(jtpnme.TRANSIENTkw))
          case VOLATILE =>
            addAnnot(scalaDot(jtpnme.VOLATILEkw))
          case SYNCHRONIZED | STRICTFP =>
            in.nextToken()
          case SEALED =>
            flags |= Flags.Sealed
            in.nextToken()
          // JEP-409: Special trick for the 'non-sealed' java keyword
          case IDENTIFIER if in.name.toString == "non" =>
            val lookahead = in.LookaheadScanner()
            ({lookahead.nextToken(); lookahead.token}, {lookahead.nextToken(); lookahead.name.toString}) match
              case (MINUS, "sealed") =>
                in.nextToken(); in.nextToken() // skip '-' and 'sealed'. Nothing more to do
              case _ =>
                syntaxError(em"Identifier '${in.name}' is not allowed here")
            in.nextToken()
          case _ =>
            val privateWithin: TypeName =
              if (isPackageAccess && !inInterface) thisPackageName
              else tpnme.EMPTY

            return Modifiers(flags, privateWithin) withAnnotations annots.toList
        }
      assert(false, "should not be here")
      throw new RuntimeException
    }

    def typeParams(flags: FlagSet = Flags.JavaDefined | Flags.PrivateLocal | Flags.Param): List[TypeDef] =
      if (in.token == LT) {
        in.nextToken()
        val tparams = repsep(() => typeParam(flags), COMMA)
        acceptClosingAngle()
        tparams
      }
      else List()

    def typeParam(flags: FlagSet): TypeDef =
      atSpan(in.offset) {
        annotations()
        val name = identForType()
        val hi = if (in.token == EXTENDS) { in.nextToken() ; bound() } else ObjectTpt()
        TypeDef(name, TypeBoundsTree(EmptyTree, hi)).withMods(Modifiers(flags))
      }

    def bound(): Tree =
      atSpan(in.offset) {
        val buf = ListBuffer[Tree](typ())
        while (in.token == AMP) {
          in.nextToken()
          buf += typ()
        }
        val ts = buf.toList
        if (ts.tail.isEmpty) ts.head
        else ts.reduce(makeAndType(_,_))
      }

    def formalParams(): List[ValDef] = {
      accept(LPAREN)
      val vparams = if (in.token == RPAREN) List() else repsep(() => formalParam(), COMMA)
      accept(RPAREN)
      vparams
    }

    def formalParam(): ValDef = {
      val start = in.offset
      if (in.token == FINAL) in.nextToken()
      annotations()
      var t = typ()
      if (in.token == DOTDOTDOT) {
        in.nextToken()
        t = atSpan(t.span.start) {
          PostfixOp(t, Ident(tpnme.raw.STAR))
        }
      }
      atSpan(start, in.offset) {
        varDecl(Modifiers(Flags.JavaDefined | Flags.Param), t, ident().toTermName)
      }
    }

    def optThrows(): Unit =
      if (in.token == THROWS) {
        in.nextToken()
        repsep(() => typ(), COMMA)
      }

    def methodBody(): Tree = atSpan(in.offset) {
      skipAhead()
      accept(RBRACE) // skip block
      unimplementedExpr
    }

    def definesInterface(token: Int): Boolean = token == INTERFACE || token == AT

    /** If the next token is the identifier "record", convert it into the RECORD token.
      * This makes it easier to handle records in various parts of the code,
      * in particular when a `parentToken` is passed to some functions.
      */
    def adaptRecordIdentifier(): Unit =
      if in.token == IDENTIFIER && in.name == jnme.RECORDid then
        in.token = RECORD

    def termDecl(start: Offset, mods: Modifiers, parentToken: Int): List[Tree] = {
      val inInterface = definesInterface(parentToken)
      val tparams = if (in.token == LT) typeParams(Flags.JavaDefined | Flags.Param) else List()
      val isVoid = in.token == VOID
      var rtpt =
        if (isVoid)
          atSpan(in.offset) {
            in.nextToken()
            TypeTree(UnitType)
          }
        else typ()
      var nameOffset = in.offset
      val rtptName = rtpt match {
        case Ident(name) => name
        case _ => nme.EMPTY
      }
      if (in.token == LPAREN && rtptName != nme.EMPTY && !inInterface) {
        // constructor declaration
        val vparams = formalParams()
        optThrows()
        List {
          atSpan(start) {
            DefDef(nme.CONSTRUCTOR, joinParams(tparams, List(vparams)),
                   TypeTree(), methodBody()).withMods(mods)
          }
        }
      } else if (in.token == LBRACE && rtptName != nme.EMPTY && parentToken == RECORD) {
        /*
        record RecordName(T param1, ...) {
          RecordName { // <- here
            // methodBody
          }
        }
        */
        methodBody()
        Nil
      }
      else {
        var mods1 = mods
        if (mods.is(Flags.Abstract)) mods1 = mods &~ Flags.Abstract
        nameOffset = in.offset
        val name = ident()
        if (in.token == LPAREN) {
          // method declaration
          val vparams = formalParams()
          if (!isVoid) rtpt = optArrayBrackets(rtpt)
          optThrows()
          val bodyOk = !inInterface || mods.isOneOf(Flags.DefaultMethod | Flags.JavaStatic | Flags.Private)
          val body =
            if (bodyOk && in.token == LBRACE)
              methodBody()
            else
              if (parentToken == AT && in.token == DEFAULT) {
                val annot =
                  atSpan(nameOffset) {
                    New(Select(Select(scalaDot(nme.annotation), nme.internal), tpnme.AnnotationDefaultATTR), Nil)
                  }
                mods1 = mods1 withAddedAnnotation annot
                val unimplemented = unimplementedExpr
                skipTo(SEMI)
                accept(SEMI)
                unimplemented
              }
              else {
                accept(SEMI)
                EmptyTree
              }
          //if (inInterface) mods1 |= Flags.Deferred
          List {
            atSpan(start, nameOffset) {
              DefDef(name.toTermName, joinParams(tparams, List(vparams)), rtpt, body).withMods(mods1 | Flags.Method)
            }
          }
        }
        else {
          if (inInterface) mods1 |= Flags.Final | Flags.JavaStatic
          val result = fieldDecls(start, nameOffset, mods1, rtpt, name)
          accept(SEMI)
          result
        }
      }
    }

    /** Parse a sequence of field declarations, separated by commas.
      *  This one is tricky because a comma might also appear in an
      *  initializer. Since we don't parse initializers we don't know
      *  what the comma signifies.
      *  We solve this with a second list buffer `maybe` which contains
      *  potential variable definitions.
      *  Once we have reached the end of the statement, we know whether
      *  these potential definitions are real or not.
      */
    def fieldDecls(start: Offset, firstNameOffset: Offset, mods: Modifiers, tpt: Tree, name: Name): List[Tree] = {
      val buf = ListBuffer[Tree](
          atSpan(start, firstNameOffset) { varDecl(mods, tpt, name.toTermName) })
      val maybe = new ListBuffer[Tree] // potential variable definitions.
      while (in.token == COMMA) {
        in.nextToken()
        if (in.token == IDENTIFIER) { // if there's an ident after the comma ...
          val nextNameOffset = in.offset
          val name = ident()
          if (in.token == EQUALS || in.token == SEMI) { // ... followed by a `=` or `;`, we know it's a real variable definition
            buf ++= maybe
            buf += atSpan(start, nextNameOffset) { varDecl(mods, tpt, name.toTermName) }
            maybe.clear()
          }
          else if (in.token == COMMA) // ... if there's a comma after the ident, it could be a real vardef or not.
            maybe += atSpan(start, nextNameOffset) { varDecl(mods, tpt, name.toTermName) }
          else { // ... if there's something else we were still in the initializer of the
            // previous var def; skip to next comma or semicolon.
            skipTo(COMMA, SEMI)
            maybe.clear()
          }
        }
        else { // ... if there's no ident following the comma we were still in the initializer of the
          // previous var def; skip to next comma or semicolon.
          skipTo(COMMA, SEMI)
          maybe.clear()
        }
      }
      if (in.token == SEMI)
        buf ++= maybe // every potential vardef that survived until here is real.
      buf.toList
    }

    def varDecl(mods: Modifiers, tpt: Tree, name: TermName): ValDef = {
      val tpt1 = optArrayBrackets(tpt)
      /** Tries to detect final static literals syntactically and returns a constant type replacement */
      def optConstantTpe(): Tree = {
        def constantTpe(const: Constant): Tree = TypeTree(ConstantType(const))

        def forConst(const: Constant): Tree = {
          if (in.token != SEMI) tpt1
          else {
            def isStringTyped = tpt1 match {
              case Ident(n: TypeName) => "String" == n.toString
              case _ => false
            }
            if (const.tag == Constants.StringTag && isStringTyped) constantTpe(const)
            else tpt1 match {
              case TypedSplice(tpt2) =>
                if (const.tag == Constants.BooleanTag || const.isNumeric) {
                  //for example, literal 'a' is ok for float. 127 is ok for byte, but 128 is not.
                  val converted = const.convertTo(tpt2.tpe)
                  if (converted == null) tpt1
                  else constantTpe(converted)
                }
                else tpt1
              case _ => tpt1
            }
          }
        }

        in.nextToken() // EQUALS
        if (mods.is(Flags.JavaStatic) && mods.is(Flags.Final)) {
          tryConstant.map(forConst).getOrElse(tpt1)
        }
        else tpt1
      }

      val tpt2: Tree =
        if (in.token == EQUALS && !mods.is(Flags.Param)) {
          val res = optConstantTpe()
          skipTo(COMMA, SEMI)
          res
        }
        else tpt1

      val mods1 = if (mods.is(Flags.Final)) mods else mods | Flags.Mutable
      ValDef(name, tpt2, if (mods.is(Flags.Param)) EmptyTree else unimplementedExpr).withMods(mods1)
    }

    def memberDecl(start: Offset, mods: Modifiers, parentToken: Int): List[Tree] = in.token match
      case CLASS | ENUM | RECORD | INTERFACE | AT =>
        typeDecl(start, if definesInterface(parentToken) then mods | Flags.JavaStatic else mods)
      case _ =>
        termDecl(start, mods, parentToken)

    def makeCompanionObject(cdef: TypeDef, statics: List[Tree]): Tree =
      atSpan(cdef.span) {
        assert(cdef.span.exists)
        ModuleDef(cdef.name.toTermName,
          makeTemplate(List(), statics, List(), needsDummyConstr = false)).withMods((cdef.mods & Flags.RetainedModuleClassFlags).toTermFlags)
      }

    def addCompanionObject(statics: List[Tree], cdef: TypeDef): List[Tree] =
      List(makeCompanionObject(cdef, statics), cdef)

    def importDecl(): List[Tree] = {
      val start = in.offset
      accept(IMPORT)
      val buf = new ListBuffer[Name]
      def collectIdents() : Int =
        if (in.token == ASTERISK) {
          val starOffset = in.offset
          in.nextToken()
          buf += nme.WILDCARD
          starOffset
        }
        else {
          val nameOffset = in.offset
          buf += ident()
          if (in.token == DOT) {
            in.nextToken()
            collectIdents()
          }
          else nameOffset
        }
      if (in.token == STATIC) in.nextToken()
      else buf += nme.ROOTPKG
      val lastnameOffset = collectIdents()
      accept(SEMI)
      val names = buf.toList
      if (names.length < 2) {
        syntaxError(start, em"illegal import", skipIt = false)
        List()
      }
      else {
        val qual = names.tail.init.foldLeft(Ident(names.head): Tree)(Select(_, _))
        val lastname = names.last
        val ident = Ident(lastname).withSpan(Span(lastnameOffset))
//        val selector = lastname match {
//          case nme.WILDCARD => Pair(ident, Ident(null) withPos Span(-1))
//          case _            => Pair(ident, ident)
//        }
        val imp = atSpan(start) { Import(qual, ImportSelector(ident) :: Nil) }
        imp :: Nil
      }
    }

    def interfacesOpt(): List[Tree] =
      if (in.token == IMPLEMENTS) {
        in.nextToken()
        repsep(() => typ(), COMMA)
      }
      else
        List()


    def permittedSubclassesOpt(isSealed: Boolean) : List[Tree] =
      if in.token == PERMITS && !isSealed then
        syntaxError(em"A type declaration that has a permits clause should have a sealed modifier")
      if in.token == PERMITS then
        in.nextToken()
        repsep(() => typ(), COMMA)
      else
        // JEP-409: Class/Interface may omit the permits clause
        Nil

    def classDecl(start: Offset, mods: Modifiers): List[Tree] = {
      accept(CLASS)
      val nameOffset = in.offset
      val name = identForType()
      val tparams = typeParams()
      val superclass =
        if (in.token == EXTENDS) {
          in.nextToken()
          typ()
        }
        else
          ObjectTpt()
      val interfaces = interfacesOpt()
      val permittedSubclasses = permittedSubclassesOpt(mods.is(Flags.Sealed))
      val (statics, body) = typeBody(CLASS, name)
      val cls = atSpan(start, nameOffset) {
        TypeDef(name, makeTemplate(superclass :: interfaces, body, tparams, needsDummyConstr = true)).withMods(mods)
      }
      addCompanionObject(statics, cls)
    }

    def recordDecl(start: Offset, mods: Modifiers): List[Tree] =
      accept(RECORD)
      val nameOffset = in.offset
      val name = identForType()
      val tparams = typeParams()
      val header = formalParams()
      val superclass = javaLangRecord() // records always extend java.lang.Record
      val interfaces = interfacesOpt() // records may implement interfaces
      val (statics, body) = typeBody(RECORD, name)

      // We need to generate accessors for every param, if no method with the same name is already defined

      var fieldsByName = header.map(v => (v.name, (v.tpt, v.mods.annotations))).to(LinkedHashMap)

      for case DefDef(name, paramss, _, _) <- body
      if paramss.isEmpty && fieldsByName.contains(name)
      do
        fieldsByName -= name
      end for

      // The `Synthetic` flag here is only used by the `Namer` to evict overriden symbols during mixed-compilation
      val accessors =
        (for (name, (tpt, annots)) <- fieldsByName yield
          DefDef(name, List(Nil), tpt, unimplementedExpr)
            .withMods(Modifiers(Flags.JavaDefined | Flags.Method | Flags.ParamAccessor | Flags.Synthetic))
        ).toList

      // generate the canonical constructor
      val canonicalConstructor =
        DefDef(nme.CONSTRUCTOR, joinParams(Nil, List(header)), TypeTree(), EmptyTree)
          .withMods(Modifiers(Flags.JavaDefined | Flags.Synthetic, mods.privateWithin))

      // return the trees
      val recordTypeDef = atSpan(start, nameOffset) {
        TypeDef(name,
          makeTemplate(
            parents = superclass :: interfaces,
            stats = canonicalConstructor :: accessors ::: body,
            tparams = tparams,
            needsDummyConstr = true
          )
        ).withMods(mods.withFlags(Flags.JavaDefined | Flags.Final))
      }
      addCompanionObject(statics, recordTypeDef)
    end recordDecl

    def interfaceDecl(start: Offset, mods: Modifiers): List[Tree] = {
      accept(INTERFACE)
      val nameOffset = in.offset
      val name = identForType()
      val tparams = typeParams()
      val parents =
        if (in.token == EXTENDS) {
          in.nextToken()
          repsep(() => typ(), COMMA)
        }
        else
          List(ObjectTpt())
      val permittedSubclasses = permittedSubclassesOpt(mods is Flags.Sealed)
      val (statics, body) = typeBody(INTERFACE, name)
      val iface = atSpan(start, nameOffset) {
        TypeDef(
          name,
          makeTemplate(parents, body, tparams, needsDummyConstr = false)).withMods(mods | Flags.JavaInterface)
      }
      addCompanionObject(statics, iface)
    }

    def typeBody(leadingToken: Int, parentName: Name): (List[Tree], List[Tree]) = {
      accept(LBRACE)
      val defs = typeBodyDecls(leadingToken, parentName)
      accept(RBRACE)
      defs
    }

    def typeBodyDecls(parentToken: Int, parentName: Name): (List[Tree], List[Tree]) = {
      val inInterface = definesInterface(parentToken)
      val statics = new ListBuffer[Tree]
      val members = new ListBuffer[Tree]
      while (in.token != RBRACE && in.token != EOF) {
        val start = in.offset
        var mods = modifiers(inInterface)
        if (in.token == LBRACE) {
          skipAhead() // skip init block, we just assume we have seen only static
          accept(RBRACE)
        }
        else if (in.token == SEMI)
          in.nextToken()
        else {
          adaptRecordIdentifier()
          if (in.token == ENUM || in.token == RECORD || definesInterface(in.token)) mods |= Flags.JavaStatic
          val decls = memberDecl(start, mods, parentToken)
          (if (mods.is(Flags.JavaStatic) || inInterface && !(decls exists (_.isInstanceOf[DefDef])))
            statics
          else
            members) ++= decls
        }
      }
      (statics.toList, members.toList)
    }
    def annotationParents: List[Tree] = List(
      ObjectTpt(),
      Select(javaLangDot(nme.annotation), tpnme.Annotation)
    )
    def annotationDecl(start: Offset, mods: Modifiers): List[Tree] = {
      accept(AT)
      accept(INTERFACE)
      val nameOffset = in.offset
      val name = identForType()
      val (statics, body) = typeBody(AT, name)
      val constructorParams = body.collect {
        case dd: DefDef =>
          makeParam(dd.name, dd.tpt)
      }
      val constr = DefDef(nme.CONSTRUCTOR,
        List(constructorParams), TypeTree(), EmptyTree).withMods(Modifiers(Flags.JavaDefined))
      val templ = makeTemplate(annotationParents, constr :: body, List(), needsDummyConstr = true)
      val annot = atSpan(start, nameOffset) {
        TypeDef(name, templ).withMods(mods | Flags.JavaInterface | Flags.JavaAnnotation)
      }
      addCompanionObject(statics, annot)
    }

    def enumDecl(start: Offset, mods: Modifiers): List[Tree] = {
      accept(ENUM)
      val nameOffset = in.offset
      val name = identForType()
      def enumType = Ident(name)
      val interfaces = interfacesOpt()
      accept(LBRACE)
      val buf = new ListBuffer[Tree]
      def parseEnumConsts(): Unit =
        if (in.token != RBRACE && in.token != SEMI && in.token != EOF) {
          buf += enumConst(enumType)
          if (in.token == COMMA) {
            in.nextToken()
            parseEnumConsts()
          }
        }
      parseEnumConsts()
      val consts = buf.toList
      val (statics, body) =
        if (in.token == SEMI) {
          in.nextToken()
          typeBodyDecls(ENUM, name)
        }
        else
          (List(), List())
      val predefs = List(
        DefDef(
          nme.values,
          ListOfNil,
          arrayOf(enumType),
          unimplementedExpr).withMods(Modifiers(Flags.JavaDefined | Flags.JavaStatic | Flags.Method)),
        DefDef(
          nme.valueOf,
          List(List(makeParam("x".toTermName, TypeTree(StringType)))),
          enumType,
          unimplementedExpr).withMods(Modifiers(Flags.JavaDefined | Flags.JavaStatic | Flags.Method)))
      accept(RBRACE)
      /*
      val superclazz =
        AppliedTypeTree(javaLangDot(tpnme.Enum), List(enumType))
        */
      val superclazz = Apply(TypeApply(
        Select(New(javaLangDot(tpnme.Enum)), nme.CONSTRUCTOR), List(enumType)), Nil)
      val enumclazz = atSpan(start, nameOffset) {
        TypeDef(name,
          makeTemplate(superclazz :: interfaces, body, List(), needsDummyConstr = true)).withMods(mods | Flags.JavaEnum)
      }
      addCompanionObject(consts ::: statics ::: predefs, enumclazz)
    }

    def enumConst(enumType: Tree): ValDef = {
      annotations()
      atSpan(in.offset) {
        val name = ident()
        if (in.token == LPAREN) {
          // skip arguments
          skipAhead()
          accept(RPAREN)
        }
        if (in.token == LBRACE) {
          // skip classbody
          skipAhead()
          accept(RBRACE)
        }
        ValDef(name.toTermName, enumType, unimplementedExpr).withMods(Modifiers(Flags.JavaEnumValue | Flags.JavaStatic))
      }
    }

    def typeDecl(start: Offset, mods: Modifiers): List[Tree] = in.token match
      case ENUM      => enumDecl(start, mods)
      case INTERFACE => interfaceDecl(start, mods)
      case AT        => annotationDecl(start, mods)
      case CLASS     => classDecl(start, mods)
      case RECORD    => recordDecl(start, mods)
      case _         => in.nextToken(); syntaxError(em"illegal start of type declaration", skipIt = true); List(errorTypeTree)

    def tryConstant: Option[Constant] = {
      val negate = in.token match {
        case MINUS | BANG => in.nextToken(); true
        case _ => false
      }
      val l = in.token match {
        case TRUE      => !negate
        case FALSE     => negate
        case CHARLIT   => in.strVal.charAt(0)
        case INTLIT    => in.intVal(negate).toInt
        case LONGLIT   => in.intVal(negate)
        case FLOATLIT  => in.floatVal(negate).toFloat
        case DOUBLELIT => in.floatVal(negate)
        case STRINGLIT => in.strVal
        case _         => null
      }
      if (l == null) None
      else {
        in.nextToken()
        Some(Constant(l))
      }
    }

    /** CompilationUnit ::= [package QualId semi] TopStatSeq
      */
    def compilationUnit(): Tree = {
      val start = in.offset
      val pkg: RefTree =
        if (in.token == AT || in.token == PACKAGE) {
          annotations()
          accept(PACKAGE)
          val pkg = qualId()
          accept(SEMI)
          pkg
        }
        else
          Ident(nme.EMPTY_PACKAGE)
      thisPackageName = convertToTypeName(pkg) match {
        case Some(t)  => t.name.toTypeName
        case _        => tpnme.EMPTY
      }
      val buf = new ListBuffer[Tree]
      while (in.token == IMPORT)
        buf ++= importDecl()
      while (in.token != EOF && in.token != RBRACE) {
        while (in.token == SEMI) in.nextToken()
        if (in.token != EOF) {
          val start = in.offset
          val mods = modifiers(inInterface = false)
          adaptRecordIdentifier() // needed for typeDecl
          buf ++= typeDecl(start, mods)
        }
      }
      val unit = atSpan(start) { PackageDef(pkg, buf.toList) }
      accept(EOF)
      unit match
        case PackageDef(Ident(nme.EMPTY_PACKAGE), Nil) => EmptyTree
        case _ => unit
    }
  }


  /** OutlineJavaParser parses top-level declarations in `source` to find declared classes, ignoring their bodies (which
   *  must only have balanced braces). This is used to map class names to defining sources.
   *  This is necessary even for Java, because the filename defining a non-public classes cannot be determined from the
   *  classname alone.
   */
  class OutlineJavaParser(source: SourceFile)(using Context) extends JavaParser(source) with OutlineParserCommon {
    override def skipBracesHook(): Option[Tree] = None
    override def typeBody(leadingToken: Int, parentName: Name): (List[Tree], List[Tree]) = {
      skipBraces()
      (List(EmptyValDef), List(EmptyTree))
    }
  }
}
