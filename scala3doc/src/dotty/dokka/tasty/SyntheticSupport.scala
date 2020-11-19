package dotty.dokka.tasty

import scala.quoted._

trait SyntheticsSupport:
  self: TastyParser =>

  import qctx.reflect._

  extension (t: TypeRepr):
    def isTupleType: Boolean = hackIsTupleType(using qctx)(t)

    def isCompiletimeAppliedType: Boolean = hackIsCompiletimeAppliedType(using qctx)(t)

    def hackIsTupleType(using QuoteContext)(rtpe: qctx.reflect.TypeRepr): Boolean =
      import dotty.tools.dotc
      given ctx as dotc.core.Contexts.Context = qctx.reflect.rootContext.asInstanceOf
      val tpe = rtpe.asInstanceOf[dotc.core.Types.Type]
      ctx.definitions.isTupleType(tpe)

    def hackIsCompiletimeAppliedType(using QuoteContext)(rtpe: qctx.reflect.TypeRepr): Boolean =
      import dotty.tools.dotc
      given ctx as dotc.core.Contexts.Context = qctx.reflect.rootContext.asInstanceOf
      val tpe = rtpe.asInstanceOf[dotc.core.Types.Type]
      ctx.definitions.isCompiletimeAppliedType(tpe.typeSymbol)

  extension (s: Symbol):
    def isSyntheticFunc: Boolean = s.flags.is(Flags.Synthetic) || s.flags.is(Flags.FieldAccessor) || isDefaultHelperMethod

    def isSuperBridgeMethod: Boolean = s.name.contains("$super$")

    def isDefaultHelperMethod: Boolean = ".*\\$default\\$\\d+$".r.matches(s.name)

    def isOpaque: Boolean = s.flags.is(Flags.Opaque)

    def isInfix: Boolean = hackIsInfix(using qctx)(s)

    def getAllMembers: List[Symbol] = hackGetAllMembers(using qctx)(s)

  def isSyntheticField(c: Symbol) =
    c.flags.is(Flags.CaseAccessor) || (c.flags.is(Flags.Object) && !c.flags.is(Flags.Given))

  def isValidPos(pos: Position) =
    pos.exists && pos.start != pos.end

  def constructorWithoutParamLists(c: ClassDef): Boolean =
    !isValidPos(c.constructor.pos)  || {
      val end = c.constructor.pos.end
      val typesEnd =  c.constructor.typeParams.lastOption.fold(end - 1)(_.pos.end)
      val classDefTree = c.constructor.show
      c.constructor.typeParams.nonEmpty && end <= typesEnd + 1
    }

  // TODO: #49 Remove it after TASTY-Reflect release with published flag Extension
  def hackIsInfix(using QuoteContext)(rsym: qctx.reflect.Symbol): Boolean = {
    import qctx.reflect._
    import dotty.tools.dotc
    given ctx as dotc.core.Contexts.Context = rootContext.asInstanceOf
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    ctx.definitions.isInfix(sym)
  }
  /* We need there to filter out symbols with certain flagsets, because these symbols come from compiler and TASTY can't handle them well.
  They are valdefs that describe case companion objects and cases from enum.
  TASTY crashed when calling _.tree on them.
  */
  def hackGetAllMembers(using QuoteContext)(rsym: qctx.reflect.Symbol): List[qctx.reflect.Symbol] = {
    import qctx.reflect._
    import dotty.tools.dotc
    given ctx as dotc.core.Contexts.Context = rootContext.asInstanceOf
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    sym.typeRef.appliedTo(sym.typeParams.map(_.typeRef)).allMembers.iterator.map(_.symbol)
      .collect {
         case sym if
          (!sym.is(dotc.core.Flags.ModuleVal) || sym.is(dotc.core.Flags.Given)) &&
          !sym.flags.isAllOf(dotc.core.Flags.Enum | dotc.core.Flags.Case | dotc.core.Flags.JavaStatic) =>
              sym.asInstanceOf[Symbol]
      }.toList
  }

  def hackGetSupertypes(using QuoteContext)(rdef: qctx.reflect.ClassDef) = {
    import qctx.reflect._
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = qctx.reflect.rootContext.asInstanceOf
    val classdef = rdef.asInstanceOf[dotc.ast.tpd.TypeDef]
    val ref = classdef.symbol.info.asInstanceOf[dotc.core.Types.ClassInfo].appliedRef
    val baseTypes: List[(dotc.core.Symbols.Symbol, dotc.core.Types.Type)] =
      ref.baseClasses.map(b => b -> ref.baseType(b))
    baseTypes.asInstanceOf[List[(Symbol, TypeRepr)]]
  }

  def getSupertypes(using QuoteContext)(c: ClassDef) = hackGetSupertypes(c).tail

  def typeForClass(c: ClassDef): TypeRepr =
    import qctx.reflect._
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = rootContext.asInstanceOf
    val cSym = c.symbol.asInstanceOf[dotc.core.Symbols.Symbol]
    cSym.typeRef.appliedTo(cSym.typeParams.map(_.typeRef)).asInstanceOf[TypeRepr]

  object MatchTypeCase:
    def unapply(tpe: TypeRepr): Option[(TypeRepr, TypeRepr)] =
      tpe match
        case AppliedType(t, Seq(from, to)) /*if t == MatchCaseType*/ =>
            Some((from, to))
        case TypeLambda(paramNames, paramTypes, AppliedType(t, Seq(from, to))) /*if t == MatchCaseType*/ =>
            Some((from, to))
        case _ =>
          None
