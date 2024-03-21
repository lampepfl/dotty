package dotty.tools
package dotc
package ast

import core.*
import Flags.*, Trees.*, Types.*, Contexts.*
import Names.*, StdNames.*, NameOps.*, Symbols.*
import typer.ConstFold
import reporting.trace

import Decorators.*
import Constants.Constant
import scala.collection.mutable

import scala.annotation.tailrec

trait TreeInfo[T <: Untyped] { self: Trees.Instance[T] =>

  def unsplice(tree: Trees.Tree[T]): Trees.Tree[T] = tree

  def isDeclarationOrTypeDef(tree: Tree): Boolean = unsplice(tree) match {
    case DefDef(_, _, _, EmptyTree)
      | ValDef(_, _, EmptyTree)
      | TypeDef(_, _) => true
    case _ => false
  }

  def isOpAssign(tree: Tree): Boolean = unsplice(tree) match {
    case Apply(fn, _ :: _) =>
      unsplice(fn) match {
        case Select(_, name) if name.isOpAssignmentName => true
        case _ => false
      }
    case _ => false
  }

  class MatchingArgs(params: List[Symbol], args: List[Tree])(using Context) {
    def foreach(f: (Symbol, Tree) => Unit): Boolean = {
      def recur(params: List[Symbol], args: List[Tree]): Boolean = params match {
        case Nil => args.isEmpty
        case param :: params1 =>
          if (param.info.isRepeatedParam) {
            for (arg <- args) f(param, arg)
            true
          }
          else args match {
            case Nil => false
            case arg :: args1 =>
              f(param, args.head)
              recur(params1, args1)
          }
      }
      recur(params, args)
    }
    def zipped: List[(Symbol, Tree)] = map((_, _))
    def map[R](f: (Symbol, Tree) => R): List[R] = {
      val b = List.newBuilder[R]
      foreach(b += f(_, _))
      b.result()
    }
  }

  /** The method part of an application node, possibly enclosed in a block
   *  with only valdefs as statements. the reason for also considering blocks
   *  is that named arguments can transform a call into a block, e.g.
   *   <init>(b = foo, a = bar)
   * is transformed to
   *   { val x$1 = foo
   *     val x$2 = bar
   *     <init>(x$2, x$1)
   *   }
   */
  def methPart(tree: Tree): Tree = stripApply(tree) match {
    case TypeApply(fn, _) => methPart(fn)
    case AppliedTypeTree(fn, _) => methPart(fn) // !!! should not be needed
    case Block(stats, expr) => methPart(expr)
    case mp => mp
  }

  /** If this is an application, its function part, stripping all
   *  Apply nodes (but leaving TypeApply nodes in). Otherwise the tree itself.
   */
  def stripApply(tree: Tree): Tree = unsplice(tree) match {
    case Apply(fn, _) => stripApply(fn)
    case _ => tree
  }

  /** If this is a block, its expression part */
  def stripBlock(tree: Tree): Tree = unsplice(tree) match {
    case Block(_, expr) => stripBlock(expr)
    case Inlined(_, _, expr) => stripBlock(expr)
    case _ => tree
  }

  def stripInlined(tree: Tree): Tree = unsplice(tree) match {
    case Inlined(_, _, expr) => stripInlined(expr)
    case _ => tree
  }

  def stripAnnotated(tree: Tree): Tree = tree match {
    case Annotated(arg, _) => arg
    case _ => tree
  }

  def stripTyped(tree: Tree): Tree = unsplice(tree) match
    case Typed(expr, _) =>
      stripTyped(expr)
    case _ =>
      tree

  /** The number of arguments in an application */
  def numArgs(tree: Tree): Int = unsplice(tree) match {
    case Apply(fn, args) => numArgs(fn) + args.length
    case TypeApply(fn, _) => numArgs(fn)
    case Block(_, expr) => numArgs(expr)
    case _ => 0
  }

  /** The type arguments of a possibly curried call */
  def typeArgss(tree: Tree): List[List[Tree]] =
    @tailrec
    def loop(tree: Tree, argss: List[List[Tree]]): List[List[Tree]] = tree match
      case TypeApply(fn, args) => loop(fn, args :: argss)
      case Apply(fn, args) => loop(fn, argss)
      case _ => argss
    loop(tree, Nil)

  /** The term arguments of a possibly curried call */
  def termArgss(tree: Tree): List[List[Tree]] =
    @tailrec
    def loop(tree: Tree, argss: List[List[Tree]]): List[List[Tree]] = tree match
      case Apply(fn, args) => loop(fn, args :: argss)
      case TypeApply(fn, args) => loop(fn, argss)
      case _ => argss
    loop(tree, Nil)

  /** All term arguments of an application in a single flattened list */
  def allArguments(tree: Tree): List[Tree] = unsplice(tree) match {
    case Apply(fn, args) => allArguments(fn) ::: args
    case TypeApply(fn, _) => allArguments(fn)
    case Block(_, expr) => allArguments(expr)
    case _ => Nil
  }

  /** Is tree explicitly parameterized with type arguments? */
  def hasExplicitTypeArgs(tree: Tree): Boolean = tree match
    case TypeApply(tycon, args) =>
      args.exists(arg => !arg.span.isZeroExtent && !tycon.span.contains(arg.span))
    case _ => false

  /** Is tree a path? */
  def isPath(tree: Tree): Boolean = unsplice(tree) match {
    case Ident(_) | This(_) | Super(_, _) => true
    case Select(qual, _) => isPath(qual)
    case _ => false
  }

  /** Is tree a self constructor call this(...)? I.e. a call to a constructor of the
   *  same object?
   */
  def isSelfConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Ident(nme.CONSTRUCTOR) | Select(This(_), nme.CONSTRUCTOR) => true
    case _ => false
  }

  /** Is tree a super constructor call?
   */
  def isSuperConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Select(Super(_, _), nme.CONSTRUCTOR) => true
    case _ => false
  }

  def isSuperSelection(tree: Tree): Boolean = unsplice(tree) match {
    case Select(Super(_, _), _) => true
    case _ => false
  }

  def isSelfOrSuperConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Ident(nme.CONSTRUCTOR)
       | Select(This(_), nme.CONSTRUCTOR)
       | Select(Super(_, _), nme.CONSTRUCTOR) => true
    case _ => false
  }

  /** Is tree a backquoted identifier or definition */
  def isBackquoted(tree: Tree): Boolean = tree.hasAttachment(Backquoted)

  /** Is tree a variable pattern? */
  def isVarPattern(pat: Tree): Boolean = unsplice(pat) match {
    case x: Ident => x.name.isVarPattern && !isBackquoted(x)
    case _  => false
  }

  /** The first constructor definition in `stats` */
  def firstConstructor(stats: List[Tree]): Tree = stats match {
    case (meth: DefDef) :: _ if meth.name.isConstructorName => meth
    case stat :: stats => firstConstructor(stats)
    case nil => EmptyTree
  }

  /** Is tpt a vararg type of the form T* or => T*? */
  def isRepeatedParamType(tpt: Tree)(using Context): Boolean = stripByNameType(tpt) match {
    case tpt: TypeTree => tpt.typeOpt.isRepeatedParam
    case AppliedTypeTree(Select(_, tpnme.REPEATED_PARAM_CLASS), _) => true
    case _ => false
  }

  /** Is this argument node of the form <expr> *, or is it a reference to
   *  such an argument ? The latter case can happen when an argument is lifted.
   */
  def isWildcardStarArg(tree: Tree)(using Context): Boolean = unbind(tree) match {
    case Typed(Ident(nme.WILDCARD_STAR), _) => true
    case Typed(_, Ident(tpnme.WILDCARD_STAR)) => true
    case Typed(_, tpt: TypeTree) => tpt.typeOpt.isRepeatedParam
    case NamedArg(_, arg) => isWildcardStarArg(arg)
    case arg => arg.typeOpt.widen.isRepeatedParam
  }

  /** Is tree a type tree of the form `=> T` or (under pureFunctions) `{refs}-> T`? */
  def isByNameType(tree: Tree)(using Context): Boolean =
    stripByNameType(tree) ne tree

  /** Strip `=> T` to `T` and (under pureFunctions) `{refs}-> T` to `T` */
  def stripByNameType(tree: Tree)(using Context): Tree = unsplice(tree) match
    case ByNameTypeTree(t1) => t1
    case _ => tree

  /** All type and value parameter symbols of this DefDef */
  def allParamSyms(ddef: DefDef)(using Context): List[Symbol] =
    ddef.paramss.flatten.map(_.symbol)

  /** Does this argument list end with an argument of the form <expr> : _* ? */
  def isWildcardStarArgList(trees: List[Tree])(using Context): Boolean =
    trees.nonEmpty && isWildcardStarArg(trees.last)

  /** Is the argument a wildcard argument of the form `_` or `x @ _`?
   */
  def isWildcardArg(tree: Tree): Boolean = unbind(tree) match {
    case Ident(nme.WILDCARD) => true
    case _                   => false
  }

  /** Does this list contain a named argument tree? */
  def hasNamedArg(args: List[Any]): Boolean = args exists isNamedArg
  val isNamedArg: Any => Boolean = (arg: Any) => arg.isInstanceOf[Trees.NamedArg[?]]

  /** Is this pattern node a catch-all (wildcard or variable) pattern? */
  def isDefaultCase(cdef: CaseDef): Boolean = cdef match {
    case CaseDef(pat, EmptyTree, _) => isWildcardArg(pat)
    case _                            => false
  }

  /** Does this CaseDef catch Throwable? */
  def catchesThrowable(cdef: CaseDef)(using Context): Boolean =
    catchesAllOf(cdef, defn.ThrowableType)

  /** Does this CaseDef catch everything of a certain Type? */
  def catchesAllOf(cdef: CaseDef, threshold: Type)(using Context): Boolean =
    isDefaultCase(cdef) ||
    cdef.guard.isEmpty && {
      unbind(cdef.pat) match {
        case Typed(Ident(nme.WILDCARD), tpt) => threshold <:< tpt.typeOpt
        case _                               => false
      }
    }

  /** Is this case guarded? */
  def isGuardedCase(cdef: CaseDef): Boolean = cdef.guard ne EmptyTree

  /** Is this parameter list a using clause? */
  def isUsingClause(params: ParamClause)(using Context): Boolean = params match
    case ValDefs(vparam :: _) =>
      val sym = vparam.symbol
      if sym.exists then sym.is(Given) else vparam.mods.is(Given)
    case _ =>
      false

  def isUsingOrTypeParamClause(params: ParamClause)(using Context): Boolean = params match
    case TypeDefs(_) => true
    case _ => isUsingClause(params)

  def isTypeParamClause(params: ParamClause)(using Context): Boolean = params match
    case TypeDefs(_) => true
    case _ => false

  private val languageSubCategories = Set(nme.experimental, nme.deprecated)

  /** If `path` looks like a language import, `Some(name)` where name
   *  is `experimental` if that sub-module is imported, and the empty
   *  term name otherwise.
   */
  def languageImport(path: Tree): Option[TermName] = path match
    case Select(p1, name: TermName) if languageSubCategories.contains(name) =>
      languageImport(p1) match
        case Some(EmptyTermName) => Some(name)
        case _ => None
    case p1: RefTree if p1.name == nme.language =>
      p1.qualifier match
        case EmptyTree => Some(EmptyTermName)
        case p2: RefTree if p2.name == nme.scala =>
          p2.qualifier match
            case EmptyTree => Some(EmptyTermName)
            case Ident(nme.ROOTPKG) => Some(EmptyTermName)
            case _ => None
        case _ => None
    case _ => None

  /** The underlying pattern ignoring any bindings */
  def unbind(x: Tree): Tree = unsplice(x) match {
    case Bind(_, y) => unbind(y)
    case y          => y
  }

  /**  The largest subset of {NoInits, PureInterface} that a
   *   trait or class with these parents can have as flags.
   */
  def parentsKind(parents: List[Tree])(using Context): FlagSet = parents match {
    case Nil => NoInitsInterface
    case Apply(_, _ :: _) :: _ | Block(_, _) :: _ => EmptyFlags
    case _ :: parents1 => parentsKind(parents1)
  }

  /** Checks whether predicate `p` is true for all result parts of this expression,
   *  where we zoom into Ifs, Matches, and Blocks.
   */
  def forallResults(tree: Tree, p: Tree => Boolean): Boolean = tree match {
    case If(_, thenp, elsep) => forallResults(thenp, p) && forallResults(elsep, p)
    case Match(_, cases) => cases forall (c => forallResults(c.body, p))
    case Block(_, expr) => forallResults(expr, p)
    case _ => p(tree)
  }

  /** The tree stripped of the possibly nested applications (term and type).
   *  The original tree if it's not an application.
   */
  def appliedCore(tree: Tree): Tree = tree match {
    case Apply(fn, _) => appliedCore(fn)
    case TypeApply(fn, _)       => appliedCore(fn)
    case AppliedTypeTree(fn, _) => appliedCore(fn)
    case tree                   => tree
  }

  /** Is tree an application with result `this.type`?
   *  Accept `b.addOne(x)` and also `xs(i) += x`
   *  where the op is an assignment operator.
   */
  def isThisTypeResult(tree: Tree)(using Context): Boolean = appliedCore(tree) match {
    case fun @ Select(receiver, op) =>
      val argss = termArgss(tree)
      tree.tpe match {
        case ThisType(tref) =>
          tref.symbol == receiver.symbol
        case tref: TermRef =>
          tref.symbol == receiver.symbol || argss.exists(_.exists(tref.symbol == _.symbol))
        case _ =>
          def checkSingle(sym: Symbol): Boolean =
            (sym == receiver.symbol) || {
              receiver match {
                case Apply(_, _) => op.isOpAssignmentName                    // xs(i) += x
                case _ => receiver.symbol != NoSymbol &&
                  (receiver.symbol.isGetter || receiver.symbol.isField)      // xs.addOne(x) for var xs
              }
            }
          @tailrec def loop(mt: Type): Boolean = mt match {
            case m: MethodType =>
              m.resType match {
                case ThisType(tref) => checkSingle(tref.symbol)
                case tref: TermRef  => checkSingle(tref.symbol)
                case restpe => loop(restpe)
              }
            case PolyType(_, restpe) => loop(restpe)
            case _ => false
          }
          fun.symbol != NoSymbol && loop(fun.symbol.info)
      }
    case _ =>
      tree.tpe.isInstanceOf[ThisType]
  }
}

trait UntypedTreeInfo extends TreeInfo[Untyped] { self: Trees.Instance[Untyped] =>
  import untpd.*

  /** The underlying tree when stripping any TypedSplice or Parens nodes */
  override def unsplice(tree: Tree): Tree = tree match {
    case TypedSplice(tree1) => tree1
    case Parens(tree1) => unsplice(tree1)
    case _ => tree
  }

  def functionWithUnknownParamType(tree: Tree): Option[Tree] = tree match {
    case Function(args, _) =>
      if (args.exists {
        case ValDef(_, tpt, _) => tpt.isEmpty
        case _ => false
      }) Some(tree)
      else None
    case Match(EmptyTree, _) =>
      Some(tree)
    case Block(Nil, expr) =>
      functionWithUnknownParamType(expr)
    case NamedArg(_, expr) =>
      functionWithUnknownParamType(expr)
    case _ =>
      None
  }

  def isFunctionWithUnknownParamType(tree: Tree): Boolean =
    functionWithUnknownParamType(tree).isDefined

  def isFunction(tree: Tree): Boolean = tree match
    case Function(_, _) | Match(EmptyTree, _) => true
    case Block(Nil, expr) => isFunction(expr)
    case _ => false

  /** Is `tree` an context function or closure, possibly nested in a block? */
  def isContextualClosure(tree: Tree)(using Context): Boolean = unsplice(tree) match {
    case tree: FunctionWithMods => tree.mods.is(Given)
    case Function((param: untpd.ValDef) :: _, _) => param.mods.is(Given)
    case Closure(_, meth, _) => true
    case Block(Nil, expr) => isContextualClosure(expr)
    case Block(DefDef(nme.ANON_FUN, params :: _, _, _) :: Nil, cl: Closure) =>
      isUsingClause(params)
    case _ => false
  }

  /**  The largest subset of {NoInits, PureInterface} that a
   *   trait or class enclosing this statement can have as flags.
   */
  private def defKind(tree: Tree)(using Context): FlagSet = unsplice(tree) match {
    case EmptyTree | _: Import => NoInitsInterface
    case tree: TypeDef => if (tree.isClassDef) NoInits else NoInitsInterface
    case tree: DefDef =>
      if tree.unforcedRhs == EmptyTree
         && tree.paramss.forall {
              case ValDefs(vparams) => vparams.forall(_.rhs.isEmpty)
              case _ => true
            }
      then
        NoInitsInterface
      else if tree.mods.is(Given) && tree.paramss.isEmpty then
        EmptyFlags // might become a lazy val: TODO: check whether we need to suppress NoInits once we have new lazy val impl
      else
        NoInits
    case tree: ValDef => if (tree.unforcedRhs == EmptyTree) NoInitsInterface else EmptyFlags
    case _ => EmptyFlags
  }

  /**  The largest subset of {NoInits, PureInterface} that a
   *   trait or class with this body can have as flags.
   */
  def bodyKind(body: List[Tree])(using Context): FlagSet =
    body.foldLeft(NoInitsInterface)((fs, stat) => fs & defKind(stat))

  /** Info of a variable in a pattern: The named tree and its type */
  type VarInfo = (NameTree, Tree)

  /** An extractor for trees of the form `id` or `id: T` */
  object IdPattern {
    def unapply(tree: Tree)(using Context): Option[VarInfo] = tree match {
      case id: Ident if id.name != nme.WILDCARD => Some(id, TypeTree())
      case Typed(id: Ident, tpt) => Some((id, tpt))
      case _ => None
    }
  }

  /** Under pureFunctions: A builder and extractor for `=> T`, which is an alias for `->{cap} T`.
   *  Only trees of the form `=> T` are matched; trees written directly as `->{cap} T`
   *  are ignored by the extractor.
   */
  object ImpureByNameTypeTree:

    def apply(tp: Tree)(using Context): untpd.ByNameTypeTree =
      untpd.ByNameTypeTree(
        untpd.CapturesAndResult(
          untpd.captureRoot.withSpan(tp.span.startPos) :: Nil, tp))

    def unapply(tp: Tree)(using Context): Option[Tree] = tp match
      case untpd.ByNameTypeTree(
        untpd.CapturesAndResult(id @ Select(_, nme.CAPTURE_ROOT) :: Nil, result))
      if id.span == result.span.startPos => Some(result)
      case _ => None
  end ImpureByNameTypeTree
}

trait TypedTreeInfo extends TreeInfo[Type] { self: Trees.Instance[Type] =>
  import TreeInfo.*
  import tpd.*

  /** The purity level of this statement.
   *  @return   Pure        if statement has no side effects
   *            Idempotent  if running the statement a second time has no side effects
   *            Impure      otherwise
   */
  def statPurity(tree: Tree)(using Context): PurityLevel = unsplice(tree) match {
    case EmptyTree
       | TypeDef(_, _)
       | Import(_, _)
       | DefDef(_, _, _, _) =>
      Pure
    case vdef @ ValDef(_, _, _) =>
      if (vdef.symbol.flags is Mutable) Impure else exprPurity(vdef.rhs) `min` Pure
    case _ =>
      Impure
      // TODO: It seem like this should be exprPurity(tree)
      // But if we do that the repl/vars test break. Need to figure out why that's the case.
  }

  /** The purity level of this expression. See docs for PurityLevel for what that means
   *
   *  Note that purity and idempotency are treated differently.
   *  References to modules and lazy vals are impure (side-effecting) both because
   *  side-effecting code may be executed and because the first reference
   *  takes a different code path than all to follow; but they are idempotent
   *  because running the expression a second time gives the cached result.
   */
  def exprPurity(tree: Tree)(using Context): PurityLevel = unsplice(tree) match {
    case EmptyTree
       | This(_)
       | Super(_, _)
       | Literal(_) =>
      PurePath
    case Ident(_) =>
      refPurity(tree)
    case Select(qual, _) =>
      if (tree.symbol.is(Erased)) Pure
      else refPurity(tree) `min` exprPurity(qual)
    case New(_) | Closure(_, _, _) =>
      Pure
    case TypeApply(fn, _) =>
      if (fn.symbol.is(Erased) || fn.symbol == defn.QuotedTypeModule_of || fn.symbol == defn.Predef_classOf) Pure else exprPurity(fn)
    case Apply(fn, args) =>
      if isPureApply(tree, fn) then
        minOf(exprPurity(fn), args.map(exprPurity)) `min` Pure
      else if fn.symbol.is(Erased) then
        Pure
      else if fn.symbol.isStableMember /* && fn.symbol.is(Lazy) */ then
        minOf(exprPurity(fn), args.map(exprPurity)) `min` Idempotent
      else
        Impure
    case Typed(expr, _) =>
      exprPurity(expr)
    case Block(stats, expr) =>
      minOf(exprPurity(expr), stats.map(statPurity))
    case Inlined(_, bindings, expr) =>
      minOf(exprPurity(expr), bindings.map(statPurity))
    case NamedArg(_, expr) =>
      exprPurity(expr)
    case _ =>
      Impure
  }

  private def minOf(l0: PurityLevel, ls: List[PurityLevel]) = ls.foldLeft(l0)(_ `min` _)

  def isPurePath(tree: Tree)(using Context): Boolean = tree.tpe match {
    case tpe: ConstantType => exprPurity(tree) >= Pure
    case _ => exprPurity(tree) == PurePath
  }

  def isPureExpr(tree: Tree)(using Context): Boolean =
    exprPurity(tree) >= Pure

  def isIdempotentPath(tree: Tree)(using Context): Boolean = tree.tpe match {
    case tpe: ConstantType => exprPurity(tree) >= Idempotent
    case _ => exprPurity(tree) >= IdempotentPath
  }

  def isIdempotentExpr(tree: Tree)(using Context): Boolean =
    exprPurity(tree) >= Idempotent

  def isPureBinding(tree: Tree)(using Context): Boolean = statPurity(tree) >= Pure

  /** Is the application `tree` with function part `fn` known to be pure?
   *  Function value and arguments can still be impure.
   */
  def isPureApply(tree: Tree, fn: Tree)(using Context): Boolean =
    def isKnownPureOp(sym: Symbol) =
      sym.owner.isPrimitiveValueClass
      || sym.owner == defn.StringClass
      || defn.pureMethods.contains(sym)

    tree.tpe.isInstanceOf[ConstantType] && tree.symbol != NoSymbol && isKnownPureOp(tree.symbol) // A constant expression with pure arguments is pure.
    || fn.symbol.isStableMember && fn.symbol.isConstructor // constructors of no-inits classes are stable

  /** The purity level of this reference.
   *  @return
   *    PurePath        if reference is (nonlazy and stable)
   *                    or to a parameterized function
   *                    or its type is a constant type
   *    IdempotentPath  if reference is lazy and stable
   *    Impure          otherwise
   *  @DarkDimius: need to make sure that lazy accessor methods have Lazy and Stable
   *               flags set.
   */
  def refPurity(tree: Tree)(using Context): PurityLevel = {
    val sym = tree.symbol
    if (!tree.hasType) Impure
    else if !tree.tpe.widen.isParameterless then PurePath
    else if sym.is(Erased) then PurePath
    else if tree.tpe.isInstanceOf[ConstantType] then PurePath
    else if (!sym.isStableMember) Impure
    else if (sym.is(Module))
      if (sym.moduleClass.isNoInitsRealClass) PurePath else IdempotentPath
    else if (sym.is(Lazy)) IdempotentPath
    else if sym.isAllOf(InlineParam) then Impure
    else PurePath
  }

  def isPureRef(tree: Tree)(using Context): Boolean =
    refPurity(tree) == PurePath
  def isIdempotentRef(tree: Tree)(using Context): Boolean =
    refPurity(tree) >= IdempotentPath

  /** (1) If `tree` is a constant expression, its value as a Literal,
   *  or `tree` itself otherwise.
   *
   *  Note: Demanding idempotency instead of purity in literalize is strictly speaking too loose.
   *  Example
   *
   *    object O { final val x = 42; println("43") }
   *    O.x
   *
   *  Strictly speaking we can't replace `O.x` with `42`.  But this would make
   *  most expressions non-constant. Maybe we can change the spec to accept this
   *  kind of eliding behavior. Or else enforce true purity in the compiler.
   *  The choice will be affected by what we will do with `inline` and with
   *  Singleton type bounds (see SIP 23). Presumably
   *
   *     object O1 { val x: Singleton = 42; println("43") }
   *     object O2 { inline val x = 42; println("43") }
   *
   *  should behave differently.
   *
   *     O1.x  should have the same effect as   { println("43"); 42 }
   *
   *  whereas
   *
   *     O2.x = 42
   *
   *  Revisit this issue once we have standardized on `inline`. Then we can demand
   *  purity of the prefix unless the selection goes to a inline val.
   *
   *  Note: This method should be applied to all term tree nodes that are not literals,
   *        that can be idempotent, and that can have constant types. So far, only nodes
   *        of the following classes qualify:
   *
   *        Ident
   *        Select
   *        TypeApply
   *
   *  (2) A primitive unary operator expression `pre.op` where `op` is one of `+`, `-`, `~`, `!`
   *  that has a constant type `ConstantType(v)` but that is not a constant expression
   *  (i.e. `pre` has side-effects) is translated to
   *
   *     { pre; v }
   *
   *  (3) An expression `pre.getClass[..]()` that has a constant type `ConstantType(v)` but where
   *  `pre` has side-effects is translated to:
   *
   *     { pre; v }
   *
   *  This avoids the situation where we have a Select node that does not have a symbol.
   */
  def constToLiteral(tree: Tree)(using Context): Tree = {
    assert(!tree.isType)
    val tree1 = ConstFold(tree)
    tree1.tpe.widenTermRefExpr.dealias.normalized match {
      case ConstantType(Constant(_: Type)) if tree.isInstanceOf[Block] =>
        // We can't rewrite `{ class A; classOf[A] }` to `classOf[A]`, so we leave
        // blocks returning a class literal alone, even if they're idempotent.
        tree1
      case ConstantType(value) =>
        def dropOp(t: Tree): Tree = t match
          case Select(pre, _) if t.tpe.isInstanceOf[ConstantType] =>
            // it's a primitive unary operator
            pre
          case Apply(TypeApply(Select(pre, nme.getClass_), _), Nil) =>
            pre
          case _ =>
            tree1

        val countsAsPure =
          if dropOp(tree1).symbol.isInlineVal
          then isIdempotentExpr(tree1)
          else isPureExpr(tree1)

        if countsAsPure then Literal(value).withSpan(tree.span)
        else
          val pre = dropOp(tree1)
          if pre eq tree1 then tree1
          else
            // it's a primitive unary operator or getClass call;
            // Simplify `pre.op` to `{ pre; v }` where `v` is the value of `pre.op`
            Block(pre :: Nil, Literal(value)).withSpan(tree.span)
      case _ => tree1
    }
  }

  def isExtMethodApply(tree: Tree)(using Context): Boolean = methPart(tree) match
    case Inlined(call, _, _) => isExtMethodApply(call)
    case tree @ Select(qual, nme.apply) => tree.symbol.is(ExtensionMethod) || isExtMethodApply(qual)
    case tree => tree.symbol.is(ExtensionMethod)

  /** Is symbol potentially a getter of a mutable variable?
   */
  def mayBeVarGetter(sym: Symbol)(using Context): Boolean = {
    def maybeGetterType(tpe: Type): Boolean = tpe match {
      case _: ExprType => true
      case tpe: MethodType => tpe.isImplicitMethod
      case tpe: PolyType => maybeGetterType(tpe.resultType)
      case _ => false
    }
    sym.owner.isClass && !sym.isStableMember && maybeGetterType(sym.info)
  }

  /** Is tree a reference to a mutable variable, or to a potential getter
   *  that has a setter in the same class?
   */
  def isVariableOrGetter(tree: Tree)(using Context): Boolean = {
    def sym = tree.symbol
    def isVar = sym.is(Mutable)
    def isGetter =
      mayBeVarGetter(sym) && sym.owner.info.member(sym.name.asTermName.setterName).exists

    unsplice(tree) match {
      case Ident(_) => isVar
      case Select(_, _) => isVar || isGetter
      case Apply(_, _) =>
        methPart(tree) match {
          case Select(qual, nme.apply) => qual.tpe.member(nme.update).exists
          case _ => false
        }
      case _ => false
    }
  }

  /** Is tree a `this` node which belongs to `enclClass`? */
  def isSelf(tree: Tree, enclClass: Symbol)(using Context): Boolean = unsplice(tree) match {
    case This(_) => tree.symbol == enclClass
    case _ => false
  }

  /** Strips layers of `.asInstanceOf[T]` / `_.$asInstanceOf[T]()` from an expression */
  def stripCast(tree: Tree)(using Context): Tree = {
    def isCast(sel: Tree) = sel.symbol.isTypeCast
    unsplice(tree) match {
      case TypeApply(sel @ Select(inner, _), _) if isCast(sel) =>
        stripCast(inner)
      case Apply(TypeApply(sel @ Select(inner, _), _), Nil) if isCast(sel) =>
        stripCast(inner)
      case t =>
        t
    }
  }

  /** The type and term arguments of a possibly curried call, in the order they are given */
  def allArgss(tree: Tree): List[List[Tree]] =
    @tailrec
    def loop(tree: Tree, argss: List[List[Tree]]): List[List[Tree]] = tree match
      case tree: GenericApply => loop(tree.fun, tree.args :: argss)
      case _ => argss
    loop(tree, Nil)

  /** The function part of a possibly curried call. Unlike `methPart` this one does
   *  not decompose blocks
   */
  def funPart(tree: Tree): Tree = tree match
    case tree: GenericApply => funPart(tree.fun)
    case tree => tree

  /** Decompose a template body into parameters and other statements */
  def decomposeTemplateBody(body: List[Tree])(using Context): (List[Tree], List[Tree]) =
    body.partition {
      case stat: TypeDef => stat.symbol is Flags.Param
      case stat: ValOrDefDef =>
        stat.symbol.is(Flags.ParamAccessor) && !stat.symbol.isSetter
      case _ => false
    }

  /** An extractor for closures, either contained in a block or standalone.
   */
  object closure {
    def unapply(tree: Tree): Option[(List[Tree], Tree, Tree)] = tree match {
      case Block(_, expr) => unapply(expr)
      case Closure(env, meth, tpt) => Some(env, meth, tpt)
      case Typed(expr, _)  => unapply(expr)
      case _ => None
    }
  }

  /** An extractor for def of a closure contained the block of the closure. */
  object closureDef {
    private def isDefaultArg(tree:Tree): Boolean = tree match
      case ValDef(_, _, Select(_,name)) if name.is(NameKinds.DefaultGetterName) => true
      case _ => false
    def unapply(tree: Tree)(using Context): Option[DefDef] = tree match {
      case Block((meth : DefDef) :: Nil, closure: Closure) if meth.symbol == closure.meth.symbol =>
        Some(meth)
      case Block(stmts, expr) if stmts.forall(isDefaultArg) =>
        unapply(expr)
      case _ =>
        None
    }
  }

  /** An extractor for the method of a closure contained the block of the closure,
   *  possibly with type ascriptions.
   */
  object possiblyTypedClosureDef:
    def unapply(tree: Tree)(using Context): Option[DefDef] = tree match
      case Typed(expr, _)  => unapply(expr)
      case _ => closureDef.unapply(tree)

  /** If tree is a closure, its body, otherwise tree itself */
  def closureBody(tree: Tree)(using Context): Tree = tree match {
    case closureDef(meth) => meth.rhs
    case _ => tree
  }

  /** Is `mdef` an eta-expansion of a method reference? To recognize this, we use
   *  the following criterion: A method definition is an eta expansion, if
   *  it contains at least one term paramter, the parameter has a zero extent span,
   *  and the right hand side is either an application or a closure with'
   *  an anonymous method that's itself characterized as an eta expansion.
   */
  def isEtaExpansion(mdef: DefDef)(using Context): Boolean =
    !rhsOfEtaExpansion(mdef).isEmpty

  def rhsOfEtaExpansion(mdef: DefDef)(using Context): Tree = mdef.paramss match
    case (param :: _) :: _ if param.asInstanceOf[Tree].span.isZeroExtent =>
      mdef.rhs match
        case rhs: Apply => rhs
        case closureDef(mdef1) => rhsOfEtaExpansion(mdef1)
        case _ => EmptyTree
    case _ => EmptyTree

  /** The variables defined by a pattern, in reverse order of their appearance. */
  def patVars(tree: Tree)(using Context): List[Symbol] = {
    val acc = new TreeAccumulator[List[Symbol]] { outer =>
      def apply(syms: List[Symbol], tree: Tree)(using Context) = tree match {
        case Bind(_, body) => apply(tree.symbol :: syms, body)
        case Annotated(tree, id @ Ident(tpnme.BOUNDTYPE_ANNOT)) => apply(id.symbol :: syms, tree)
        case QuotePattern(bindings, body, _) => quotePatVars(bindings.map(_.symbol) ::: syms, body)
        case _ => foldOver(syms, tree)
      }
      private object quotePatVars extends TreeAccumulator[List[Symbol]] {
        def apply(syms: List[Symbol], tree: Tree)(using Context) = tree match {
          case SplicePattern(pat, _) => outer.apply(syms, pat)
          case _ => foldOver(syms, tree)
        }
      }
    }
    acc(Nil, tree)
  }

  /** Is this pattern node a catch-all or type-test pattern? */
  def isCatchCase(cdef: CaseDef)(using Context): Boolean = cdef match {
    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), EmptyTree, _) =>
      isSimpleThrowable(tpt.tpe)
    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), EmptyTree, _) =>
      isSimpleThrowable(tpt.tpe)
    case _ =>
      isDefaultCase(cdef)
  }

  private def isSimpleThrowable(tp: Type)(using Context): Boolean = tp match {
    case tp @ TypeRef(pre, _) =>
      (pre == NoPrefix || pre.typeSymbol.isStatic) &&
      (tp.symbol derivesFrom defn.ThrowableClass) && !tp.symbol.is(Trait)
    case _ =>
      false
  }

  /** The symbols defined locally in a statement list */
  def localSyms(stats: List[Tree])(using Context): List[Symbol] =
    if stats.isEmpty then Nil
    else
      val locals = new mutable.ListBuffer[Symbol]
      for stat <- stats do
        if stat.isDef && stat.symbol.exists then locals += stat.symbol
      locals.toList

  /** If `tree` is a DefTree, the symbol defined by it, otherwise NoSymbol */
  def definedSym(tree: Tree)(using Context): Symbol =
    if (tree.isDef) tree.symbol else NoSymbol

  /** Going from child to parent, the path of tree nodes that starts
   *  with a definition of symbol `sym` and ends with `root`, or Nil
   *  if no such path exists.
   *  Pre: `sym` must have a position.
   */
  def defPath(sym: Symbol, root: Tree)(using Context): List[Tree] = trace.onDebug(s"defpath($sym with position ${sym.span}, ${root.show})") {
    require(sym.span.exists, sym)
    object accum extends TreeAccumulator[List[Tree]] {
      def apply(x: List[Tree], tree: Tree)(using Context): List[Tree] =
        if (tree.span.contains(sym.span))
          if (definedSym(tree) == sym) tree :: x
          else {
            val x1 = foldOver(x, tree)
            if (x1 ne x) tree :: x1 else x1
          }
        else x
    }
    accum(Nil, root)
  }

  /** The top level classes in this tree, including only those module classes that
   *  are not a linked class of some other class in the result.
   */
  def topLevelClasses(tree: Tree)(using Context): List[ClassSymbol] = tree match {
    case PackageDef(_, stats) => stats.flatMap(topLevelClasses)
    case tdef: TypeDef if tdef.symbol.isClass => tdef.symbol.asClass :: Nil
    case _ => Nil
  }

  /** The tree containing only the top-level classes and objects matching either `cls` or its companion object */
  def sliceTopLevel(tree: Tree, cls: ClassSymbol)(using Context): List[Tree] = tree match {
    case PackageDef(pid, stats) =>
      val slicedStats = stats.flatMap(sliceTopLevel(_, cls))
      val isEffectivelyEmpty = slicedStats.forall(_.isInstanceOf[Import])
      if isEffectivelyEmpty then Nil
      else cpy.PackageDef(tree)(pid, slicedStats) :: Nil
    case tdef: TypeDef =>
      val sym = tdef.symbol
      assert(sym.isClass)
      if (cls == sym || cls == sym.linkedClass) tdef :: Nil
      else Nil
    case vdef: ValDef =>
      val sym = vdef.symbol
      assert(sym.is(Module))
      if (cls == sym.companionClass || cls == sym.moduleClass) vdef :: Nil
      else Nil
    case tree =>
      tree :: Nil
  }

  /** The statement sequence that contains a definition of `sym`, or Nil
   *  if none was found.
   *  For a tree to be found, The symbol must have a position and its definition
   *  tree must be reachable from come tree stored in an enclosing context.
   */
  def definingStats(sym: Symbol)(using Context): List[Tree] =
    if (!sym.span.exists || (ctx eq NoContext) || (ctx.compilationUnit eq NoCompilationUnit)) Nil
    else defPath(sym, ctx.compilationUnit.tpdTree) match {
      case defn :: encl :: _ =>
        def verify(stats: List[Tree]) =
          if (stats exists (definedSym(_) == sym)) stats else Nil
        encl match {
          case Block(stats, _) => verify(stats)
          case encl: Template => verify(encl.body)
          case PackageDef(_, stats) => verify(stats)
          case _ => Nil
        }
      case nil =>
        Nil
    }

  /** If `tree` is an instance of `TupleN[...](e1, ..., eN)`, the arguments `e1, ..., eN`
   *  otherwise the empty list.
   */
  def tupleArgs(tree: Tree)(using Context): List[Tree] = tree match {
    case Block(Nil, expr) => tupleArgs(expr)
    case Inlined(_, Nil, expr) => tupleArgs(expr)
    case Apply(fn: NameTree, args)
    if fn.name == nme.apply &&
        fn.symbol.owner.is(Module) &&
        defn.isTupleClass(fn.symbol.owner.companionClass) => args
    case _ => Nil
  }

  /** The qualifier part of a Select or Ident.
   *  For an Ident, this is the `This` of the current class.
   */
  def qualifier(tree: Tree)(using Context): Tree = tree match {
    case Select(qual, _) => qual
    case tree: Ident => desugarIdentPrefix(tree)
    case _ => This(ctx.owner.enclosingClass.asClass)
  }

  /** Is this a (potentially applied) selection of a member of a structural type
   *  that is not a member of an underlying class or trait?
   */
  def isStructuralTermSelectOrApply(tree: Tree)(using Context): Boolean = {
    def isStructuralTermSelect(tree: Select) =
      def hasRefinement(qualtpe: Type): Boolean = qualtpe.dealias match
        case defn.FunctionTypeOfMethod(_) =>
          false
        case tp: MatchType =>
          hasRefinement(tp.tryNormalize)
        case RefinedType(parent, rname, rinfo) =>
          rname == tree.name || hasRefinement(parent)
        case tp: TypeProxy =>
          hasRefinement(tp.superType)
        case tp: AndType =>
          hasRefinement(tp.tp1) || hasRefinement(tp.tp2)
        case tp: OrType =>
          hasRefinement(tp.tp1) || hasRefinement(tp.tp2)
        case _ =>
          false
      !tree.symbol.exists
      && tree.isTerm
      && hasRefinement(tree.qualifier.tpe)
    funPart(tree) match
      case tree: Select =>
        isStructuralTermSelect(tree)
      case _ =>
        false
  }

  /** Return a pair consisting of (supercall, rest)
   *
   *  - supercall: the superclass call, excluding trait constr calls
   *
   *  The supercall is always the first statement (if it exists)
   */
  final def splitAtSuper(constrStats: List[Tree])(implicit ctx: Context): (List[Tree], List[Tree]) =
    constrStats.toList match {
      case (sc: Apply) :: rest if sc.symbol.isConstructor => (sc :: Nil, rest)
      case (block @ Block(_, sc: Apply)) :: rest if sc.symbol.isConstructor => (block :: Nil, rest)
      case stats => (Nil, stats)
    }

  /** Structural tree comparison (since == on trees is reference equality).
   *  For the moment, only Ident, Select, Literal, Apply and TypeApply are supported
   */
  extension (t1: Tree) {
    def === (t2: Tree)(using Context): Boolean = (t1, t2) match {
      case (t1: Ident, t2: Ident) =>
        t1.symbol == t2.symbol
      case (t1 @ Select(q1, _), t2 @ Select(q2, _)) =>
        t1.symbol == t2.symbol && q1 === q2
      case (Literal(c1), Literal(c2)) =>
        c1 == c2
      case (Apply(f1, as1), Apply(f2, as2)) =>
        f1 === f2 && as1.corresponds(as2)(_ === _)
      case (TypeApply(f1, ts1), TypeApply(f2, ts2)) =>
        f1 === f2 && ts1.tpes.corresponds(ts2.tpes)(_ =:= _)
      case _ =>
        false
    }
    def hash(using Context): Int =
      t1.getClass.hashCode * 37 + {
        t1 match {
          case t1: Ident => t1.symbol.hashCode
          case t1 @ Select(q1, _) => t1.symbol.hashCode * 41 + q1.hash
          case Literal(c1) => c1.hashCode
          case Apply(f1, as1) => as1.foldLeft(f1.hash)((h, arg) => h * 41 + arg.hash)
          case TypeApply(f1, ts1) => ts1.foldLeft(f1.hash)((h, arg) => h * 41 + arg.tpe.hash)
          case _ => t1.hashCode
        }
      }
  }

  def assertAllPositioned(tree: Tree)(using Context): Unit =
    tree.foreachSubTree {
      case t: WithoutTypeOrPos[?] =>
      case t => assert(t.span.exists, i"$t")
    }

  object QuotedTypeOf {
    /** Extracts the content of a quoted tree.
     *  The result can be the contents of a term or type quote, which
     *  will return a term or type tree respectively.
     */
    def unapply(tree: tpd.Apply)(using Context): Option[tpd.Tree] =
      if tree.symbol == defn.QuotedTypeModule_of then
        // quoted.Type.of[<body>](quotes)
        val TypeApply(_, body :: _) = tree.fun: @unchecked
        Some(body)
      else None
  }

  /** Extractors for type splices */
  object SplicedType {
    /** Extracts the content of a spliced type tree.
      *  The result can be the contents of a type splice, which
      *  will return a type tree.
      */
    def unapply(tree: tpd.Select)(using Context): Option[tpd.Tree] =
      if tree.symbol.isTypeSplice then Some(tree.qualifier) else None
  }

  extension (tree: tpd.Quote)
    /** Type of the quoted expression as seen from outside the quote */
    def bodyType(using Context): Type =
      val quoteType = tree.tpe // `Quotes ?=> Expr[T]` or `Quotes ?=> Type[T]`
      val exprType = quoteType.argInfos.last // `Expr[T]` or `Type[T]`
      exprType.argInfos.head // T
  end extension

  extension (tree: tpd.QuotePattern)
    /** Type of the quoted pattern */
    def bodyType(using Context): Type =
      val quoteType = tree.tpe // `Expr[T]` or `Type[T]`
      quoteType.argInfos.head // T
  end extension

  /** Extractor for not-null assertions.
   *  A not-null assertion for reference `x` has the form `x.$asInstanceOf$[x.type & T]`.
   */
  object AssertNotNull :
    def apply(tree: tpd.Tree, tpnn: Type)(using Context): tpd.Tree =
      tree.select(defn.Any_typeCast).appliedToType(AndType(tree.tpe, tpnn))

    def unapply(tree: tpd.TypeApply)(using Context): Option[tpd.Tree] = tree match
      case TypeApply(Select(qual: RefTree, nme.asInstanceOfPM), arg :: Nil) =>
        arg.tpe match
          case AndType(ref, nn1) if qual.tpe eq ref =>
            qual.tpe.widen match
              case OrNull(nn2) if nn1 eq nn2 =>
              	Some(qual)
              case _ => None
          case _ => None
      case _ => None
  end AssertNotNull

  object ConstantValue {
    def unapply(tree: Tree)(using Context): Option[Any] =
      tree match
        case Typed(expr, _) => unapply(expr)
        case Inlined(_, Nil, expr) => unapply(expr)
        case Block(Nil, expr) => unapply(expr)
        case _ =>
          tree.tpe.widenTermRefExpr.dealias.normalized match
            case ConstantType(Constant(x)) => Some(x)
            case _ => None
  }
}

object TreeInfo {
  /** A purity level is represented as a bitset (expressed as an Int) */
  class PurityLevel(val x: Int) extends AnyVal {
    /** `this` contains the bits of `that` */
    def >= (that: PurityLevel): Boolean = (x & that.x) == that.x

    /** The intersection of the bits of `this` and `that` */
    def min(that: PurityLevel): PurityLevel = new PurityLevel(x & that.x)
  }

  /** An expression is a stable path. Requires that expression is at least idempotent */
  val Path: PurityLevel = new PurityLevel(4)

  /** The expression has no side effects */
  val Pure: PurityLevel = new PurityLevel(3)

  /** Running the expression a second time has no side effects. Implied by `Pure`. */
  val Idempotent: PurityLevel = new PurityLevel(1)

  val Impure: PurityLevel = new PurityLevel(0)

  /** A stable path that is evaluated without side effects */
  val PurePath: PurityLevel = new PurityLevel(Pure.x | Path.x)

  /** A stable path that is also idempotent */
  val IdempotentPath: PurityLevel = new PurityLevel(Idempotent.x | Path.x)
}
