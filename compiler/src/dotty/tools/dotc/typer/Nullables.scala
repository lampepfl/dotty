package dotty.tools
package dotc
package typer

import core._
import Types._, Contexts._, Symbols._, Decorators._, Constants._
import annotation.tailrec
import StdNames.nme
import util.Property
import Names.Name
import util.Spans.Span
import Flags._
import NullOpsDecorator._
import collection.mutable
import config.Printers.nullables
import ast.{tpd, untpd}
import ast.Trees.mods

/** Operations for implementing a flow analysis for nullability */
object Nullables:
  import ast.tpd._

  def importUnsafeNulls(using Context): Import = Import(
    ref(defn.LanguageModule),
    List(untpd.ImportSelector(untpd.Ident(nme.unsafeNulls), EmptyTree, EmptyTree)))

  inline def unsafeNullsEnabled(using Context): Boolean =
    ctx.explicitNulls && !ctx.mode.is(Mode.SafeNulls)

  private def needNullifyHi(lo: Type, hi: Type)(using Context): Boolean =
    ctx.mode.is(Mode.SafeNulls)
    && lo.isExactlyNull // only nullify hi if lo is exactly Null type
    && hi.isValueType
    // We cannot check if hi is nullable, because it can cause cyclic reference.

  /** Create a nullable type bound
   *  If lo is `Null`, `| Null` is added to hi
   */
  def createNullableTypeBounds(lo: Type, hi: Type)(using Context): TypeBounds =
    val newHi = if needNullifyHi(lo, hi) then OrType(hi, defn.NullType, soft = false) else hi
    TypeBounds(lo, newHi)

  /** Create a nullable type bound tree
   *  If lo is `Null`, `| Null` is added to hi
   */
  def createNullableTypeBoundsTree(lo: Tree, hi: Tree, alias: Tree = EmptyTree)(using Context): TypeBoundsTree =
    val hiTpe = hi.typeOpt
    val newHi = if needNullifyHi(lo.typeOpt, hiTpe) then TypeTree(OrType(hiTpe, defn.NullType, soft = false)) else hi
    TypeBoundsTree(lo, newHi, alias)

  /** A set of val or var references that are known to be not null, plus a set of
   *  variable references that are not known (anymore) to be not null
   */
  case class NotNullInfo(asserted: Set[TermRef], retracted: Set[TermRef]):
    assert((asserted & retracted).isEmpty)

    def isEmpty = this eq NotNullInfo.empty

    def retractedInfo = NotNullInfo(Set(), retracted)

    /** The sequential combination with another not-null info */
    def seq(that: NotNullInfo): NotNullInfo =
      if this.isEmpty then that
      else if that.isEmpty then this
      else NotNullInfo(
        this.asserted.union(that.asserted).diff(that.retracted),
        this.retracted.union(that.retracted).diff(that.asserted))

    /** The alternative path combination with another not-null info. Used to merge
     *  the nullability info of the two branches of an if.
     */
    def alt(that: NotNullInfo): NotNullInfo =
      NotNullInfo(this.asserted.intersect(that.asserted), this.retracted.union(that.retracted))

  object NotNullInfo:
    val empty = new NotNullInfo(Set(), Set())
    def apply(asserted: Set[TermRef], retracted: Set[TermRef]): NotNullInfo =
      if asserted.isEmpty && retracted.isEmpty then empty
      else new NotNullInfo(asserted, retracted)
  end NotNullInfo

  /** A pair of not-null sets, depending on whether a condition is `true` or `false` */
  case class NotNullConditional(ifTrue: Set[TermRef], ifFalse: Set[TermRef]):
    def isEmpty = this eq NotNullConditional.empty

  object NotNullConditional:
    val empty = new NotNullConditional(Set(), Set())
    def apply(ifTrue: Set[TermRef], ifFalse: Set[TermRef]): NotNullConditional =
      if ifTrue.isEmpty && ifFalse.isEmpty then empty
      else new NotNullConditional(ifTrue, ifFalse)
  end NotNullConditional

  /** An attachment that represents conditional flow facts established
   *  by this tree, which represents a condition.
   */
  private[typer] val NNConditional = Property.StickyKey[NotNullConditional]

   /** An attachment that represents unconditional flow facts established
    *  by this tree.
    */
  private[typer] val NNInfo = Property.StickyKey[NotNullInfo]

  /** An extractor for null comparisons */
  object CompareNull:

    /** Matches one of
     *
     *    tree == null, tree eq null, null == tree, null eq tree
     *    tree != null, tree ne null, null != tree, null ne tree
     *
     *  The second boolean result is true for equality tests, false for inequality tests
     */
    def unapply(tree: Tree)(using Context): Option[(Tree, Boolean)] = tree match
      case Apply(Select(l, _), Literal(Constant(null)) :: Nil) =>
        testSym(tree.symbol, l)
      case Apply(Select(Literal(Constant(null)), _), r :: Nil) =>
        testSym(tree.symbol, r)
      case _ =>
        None

    private def testSym(sym: Symbol, operand: Tree)(using Context) =
      if sym == defn.Any_== || sym == defn.Object_eq then Some((operand, true))
      else if sym == defn.Any_!= || sym == defn.Object_ne then Some((operand, false))
      else None

  end CompareNull

  /** An extractor for null-trackable references */
  object TrackedRef:
    def unapply(tree: Tree)(using Context): Option[TermRef] = tree.typeOpt match
      case ref: TermRef if isTracked(ref) => Some(ref)
      case _ => None
  end TrackedRef

  /** Is the given reference tracked for nullability?
   *
   *  This is the case if one of the following holds:
   *  1) The reference is a path to an immutable `val`.
   *  2) The reference is to a mutable variable, in which case all assignments to it must be
   *    reachable (in the sense of how it is defined in assignmentSpans) _and_ the variable
   *    must not be used "out of order" (in the sense specified by `usedOutOfOrder`).
   *
   *  Whether to track a local mutable variable during flow typing?
   *  We track a local mutable variable iff the variable is not assigned in a closure.
   *  For example, in the following code `x` is assigned to by the closure `y`, so we do not
   *  do flow typing on `x`.
   *
   *  ```scala
   *  var x: String|Null = ???
   *  def y = {
   *    x = null
   *  }
   *  if (x != null) {
   *    // y can be called here, which break the fact
   *    val a: String = x // error: x is captured and mutated by the closure, not trackable
   *  }
   *  ```
   *
   *  Check `usedOutOfOrder` to see the explaination and example of "out of order".
   *  See more examples in `tests/explicit-nulls/neg/var-ref-in-closure.scala`.
   */
  def isTracked(ref: TermRef)(using Context) =
    ref.isStable
    || { val sym = ref.symbol
         val unit = ctx.compilationUnit
         !ref.usedOutOfOrder
         && sym.span.exists
         && (unit ne NoCompilationUnit) // could be null under -Ytest-pickler
         && unit.assignmentSpans.contains(sym.span.start)
      }

  /** The nullability context to be used after a case that matches pattern `pat`.
   *  If `pat` is `null`, this will assert that the selector `sel` is not null afterwards.
   */
  def afterPatternContext(sel: Tree, pat: Tree)(using Context) = (sel, pat) match
    case (TrackedRef(ref), Literal(Constant(null))) => ctx.addNotNullRefs(Set(ref))
    case _ => ctx

  /** The nullability context to be used for the guard and rhs of a case with
   *  given pattern `pat`. If the pattern can only match non-null values, this
   *  will assert that the selector `sel` is not null in these regions.
   */
  def caseContext(sel: Tree, pat: Tree)(using Context): Context = sel match
    case TrackedRef(ref) if matchesNotNull(pat) => ctx.addNotNullRefs(Set(ref))
    case _ => ctx

  private def matchesNotNull(pat: Tree)(using Context): Boolean = pat match
    case _: Typed | _: UnApply => true
    case Alternative(pats) => pats.forall(matchesNotNull)
    // TODO: Add constant pattern if the constant type is not nullable
    case _ => false

  extension (infos: List[NotNullInfo])

    /** Do the current not-null infos imply that `ref` is not null?
    *  Not-null infos are as a history where earlier assertions and retractions replace
    *  later ones (i.e. it records the assignment history in reverse, with most recent first)
    */
    @tailrec def impliesNotNull(ref: TermRef): Boolean = infos match
      case info :: infos1 =>
        if info.asserted.contains(ref) then true
        else if info.retracted.contains(ref) then false
        else infos1.impliesNotNull(ref)
      case _ =>
        false

    /** Add `info` as the most recent entry to the list of null infos. Assertions
    *  or retractions in `info` supersede infos in existing entries of `infos`.
    */
    def extendWith(info: NotNullInfo) =
      if info.isEmpty
        || info.asserted.forall(infos.impliesNotNull(_))
            && !info.retracted.exists(infos.impliesNotNull(_))
      then infos
      else info :: infos

    /** Retract all references to mutable variables */
    def retractMutables(using Context) =
      val mutables = infos.foldLeft(Set[TermRef]())((ms, info) =>
        ms.union(info.asserted.filter(_.symbol.is(Mutable))))
      infos.extendWith(NotNullInfo(Set(), mutables))

  end extension

  extension (ref: TermRef)

    /** Is the use of a mutable variable out of order
    *
    *  Whether to generate and use flow typing on a specific _use_ of a local mutable variable?
    *  We only want to do flow typing on a use that belongs to the same method as the definition
    *  of the local variable.
    *  For example, in the following code, even `x` is not assigned to by a closure, but we can only
    *  use flow typing in one of the occurrences (because the other occurrence happens within a nested
    *  closure).
    *  ```scala
    *  var x: String|Null = ???
    *  def y = {
    *    if (x != null) {
    *      // not safe to use the fact (x != null) here
    *      // since y can be executed at the same time as the outer block
    *      val _: String = x
    *    }
    *  }
    *  if (x != null) {
    *    val a: String = x // ok to use the fact here
    *    x = null
    *  }
    *  ```
    *
    *  Another example:
    *  ```scala
    *  var x: String|Null = ???
    *  if (x != null) {
    *    def f: String = {
    *      val y: String = x // error: the use of x is out of order
    *      y
    *    }
    *    x = null
    *    val y: String = f // danger
    *  }
    *  ```
    */
    def usedOutOfOrder(using Context): Boolean =
      val refSym = ref.symbol
      val refOwner = refSym.owner

      @tailrec def recur(s: Symbol): Boolean =
        s != NoSymbol
        && s != refOwner
        && (s.isOneOf(MethodOrLazy) // not at the rhs of lazy ValDef or in a method (or lambda)
            || s.isClass // not in a class
            || recur(s.owner))

      refSym.is(Mutable) // if it is immutable, we don't need to check the rest conditions
      && refOwner.isTerm
      && recur(ctx.owner)
  end extension

  extension (tree: Tree)

    /* The `tree` with added nullability attachment */
    def withNotNullInfo(info: NotNullInfo): tree.type =
      if !info.isEmpty then tree.putAttachment(NNInfo, info)
      tree

    /* The nullability info of `tree` */
    def notNullInfo(using Context): NotNullInfo =
      stripInlined(tree).getAttachment(NNInfo) match
        case Some(info) if !ctx.erasedTypes => info
        case _ => NotNullInfo.empty

    /* The nullability info of `tree`, assuming it is a condition that evaluates to `c` */
    def notNullInfoIf(c: Boolean)(using Context): NotNullInfo =
      val cond = tree.notNullConditional
      if cond.isEmpty then tree.notNullInfo
      else tree.notNullInfo.seq(NotNullInfo(if c then cond.ifTrue else cond.ifFalse, Set()))

    /** The paths that are known to be not null if the condition represented
    *  by `tree` yields `true` or `false`. Two empty sets if `tree` is not
    *  a condition.
    */
    def notNullConditional(using Context): NotNullConditional =
      stripBlock(tree).getAttachment(NNConditional) match
        case Some(cond) if !ctx.erasedTypes => cond
        case _ => NotNullConditional.empty

    /** The current context augmented with nullability information of `tree` */
    def nullableContext(using Context): Context =
      val info = tree.notNullInfo
      if info.isEmpty then ctx else ctx.addNotNullInfo(info)

    /** The current context augmented with nullability information,
    *  assuming the result of the condition represented by `tree` is the same as
    *  the value of `c`.
    */
    def nullableContextIf(c: Boolean)(using Context): Context =
      val info = tree.notNullInfoIf(c)
      if info.isEmpty then ctx else ctx.addNotNullInfo(info)

    /** The context to use for the arguments of the function represented by `tree`.
    *  This is the current context, augmented with nullability information
    *  of the left argument, if the application is a boolean `&&` or `||`.
    */
    def nullableInArgContext(using Context): Context = tree match
      case Select(x, _) if !ctx.erasedTypes =>
        if tree.symbol == defn.Boolean_&& then x.nullableContextIf(true)
        else if tree.symbol == defn.Boolean_|| then x.nullableContextIf(false)
        else ctx
      case _ => ctx

    /** The `tree` augmented with nullability information in an attachment.
    *  The following operations lead to nullability info being recorded:
    *
    *  1. Null tests using `==`, `!=`, `eq`, `ne`, if the compared entity is
    *     a path (i.e. a stable TermRef)
    *  2. Boolean &&, ||, !
    */
    def computeNullable()(using Context): tree.type =
      def setConditional(ifTrue: Set[TermRef], ifFalse: Set[TermRef]) =
        tree.putAttachment(NNConditional, NotNullConditional(ifTrue, ifFalse))
      if !ctx.erasedTypes && analyzedOps.contains(tree.symbol.name.toTermName) then
        tree match
          case CompareNull(TrackedRef(ref), testEqual) =>
            if testEqual then setConditional(Set(), Set(ref))
            else setConditional(Set(ref), Set())
          case Apply(Select(x, _), y :: Nil) =>
            val xc = x.notNullConditional
            val yc = y.notNullConditional
            if !(xc.isEmpty && yc.isEmpty) then
              if tree.symbol == defn.Boolean_&& then
                setConditional(xc.ifTrue | yc.ifTrue, xc.ifFalse & yc.ifFalse)
              else if tree.symbol == defn.Boolean_|| then
                setConditional(xc.ifTrue & yc.ifTrue, xc.ifFalse | yc.ifFalse)
          case Select(x, _) if tree.symbol == defn.Boolean_! =>
            val xc = x.notNullConditional
            if !xc.isEmpty then
              setConditional(xc.ifFalse, xc.ifTrue)
          case _ =>
      tree

    /** Compute nullability information for this tree and all its subtrees */
    def computeNullableDeeply()(using Context): Unit =
      new TreeTraverser {
        def traverse(tree: Tree)(using Context) =
          traverseChildren(tree)
          tree.computeNullable()
      }.traverse(tree)
  end extension

  extension (tree: Assign)
    def computeAssignNullable()(using Context): tree.type = tree.lhs match
      case TrackedRef(ref) =>
        val rhstp = tree.rhs.typeOpt
        if ctx.explicitNulls && ref.isNullableUnion then
          if rhstp.isNullType || rhstp.isNullableUnion then
            // If the type of rhs is nullable (`T|Null` or `Null`), then the nullability of the
            // lhs variable is no longer trackable. We don't need to check whether the type `T`
            // is correct here, as typer will check it.
            tree.withNotNullInfo(NotNullInfo(Set(), Set(ref)))
          else
            // If the initial type is nullable and the assigned value is non-null,
            // we add it to the NotNull.
            tree.withNotNullInfo(NotNullInfo(Set(ref), Set()))
        else tree
      case _ => tree
  end extension

  private val analyzedOps = Set(nme.EQ, nme.NE, nme.eq, nme.ne, nme.ZAND, nme.ZOR, nme.UNARY_!)

  /** A map from (name-) offsets of all local variables in this compilation unit
   *  that can be tracked for being not null to the list of spans of assignments
   *  to these variables. A variable can be tracked if it has only reachable assignments
   *  An assignment is reachable if the path of tree nodes between the block enclosing
   *  the variable declaration to the assignment consists only of if-expressions,
   *  while-expressions, block-expressions and type-ascriptions.
   *  Only reachable assignments are handled correctly in the nullability analysis.
   *  Therefore, variables with unreachable assignments can be assumed to be not-null
   *  only if their type asserts it.
   *
   *  Note: we track the local variables through their offset and not through their name
   *  because of shadowing.
   */
  def assignmentSpans(using Context): Map[Int, List[Span]] =
    import ast.untpd._

    object populate extends UntypedTreeTraverser:

      /** The name offsets of variables that are tracked */
      var tracked: Map[Int, List[Span]] = Map.empty

      /** Map the names of potentially trackable candidate variables in scope to the spans
       *  of their reachable assignments
       */
      val candidates = mutable.Map[Name, List[Span]]()

      /** An assignment to a variable that's not in reachable makes the variable
       *  ineligible for tracking
       */
      var reachable: Set[Name] = Set.empty

      def traverse(tree: Tree)(using Context) =
        val savedReachable = reachable
        tree match
          case Block(stats, expr) =>
            var shadowed: Set[(Name, List[Span])] = Set.empty
            for stat <- stats do
              stat match
                case stat: ValDef if stat.mods.is(Mutable) =>
                  for prevSpans <- candidates.put(stat.name, Nil) do
                    shadowed += (stat.name -> prevSpans)
                  reachable += stat.name
                case _ =>
            traverseChildren(tree)
            for stat <- stats do
              stat match
                case stat: ValDef if stat.mods.is(Mutable) =>
                  for spans <- candidates.remove(stat.name) do
                    tracked += (stat.nameSpan.start -> spans) // candidates that survive until here are tracked
                case _ =>
            candidates ++= shadowed
          case Assign(Ident(name), rhs) =>
            candidates.get(name) match
              case Some(spans) =>
                if reachable.contains(name) then candidates(name) = tree.span :: spans
                else candidates -= name
              case None =>
            traverseChildren(tree)
          case _: (If | WhileDo | Typed) =>
            traverseChildren(tree)      // assignments to candidate variables are OK here ...
          case _ =>
            reachable = Set.empty       // ... but not here
            traverseChildren(tree)
        reachable = savedReachable

    populate.traverse(ctx.compilationUnit.untpdTree)
    populate.tracked
  end assignmentSpans

  /** The initial context to be used for a while expression with given span.
   *  In this context, all variables that are assigned within the while expression
   *  have their nullability status retracted, i.e. are not known to be not null.
   *  While necessary for soundness, this scheme loses precision: Even if
   *  the initial state of the variable is not null and all assignments to the variable
   *  in the while expression are also known to be not null, the variable is still
   *  assumed to be potentially null. The loss of precision is unavoidable during
   *  normal typing, since we can only do a linear traversal which does not allow
   *  a fixpoint computation. But it could be mitigated as follows:
   *
   *   - initially, use `whileContext` as computed here
   *   - when typechecking the while, delay all errors due to a variable being potentially null
   *   - afterwards, if there are such delayed errors, run the analysis again with
   *     as a fixpoint computation, reporting all previously delayed errors that remain.
   *
   *  The following code would produce an error in the current analysis, but not in the
   *  refined analysis:
   *
   *     class Links(val elem: T, val next: Links | Null)
   *
   *     var xs: Links | Null = Links(1, null)
   *     var ys: Links | Null = xs
   *     while xs != null
   *       ys = Links(xs.elem, ys.next)  // error in unrefined: ys is potentially null here
   *       xs = xs.next
   */
  def whileContext(whileSpan: Span)(using Context): Context =

    def isRetracted(ref: TermRef): Boolean =
      val sym = ref.symbol
      sym.span.exists
      && assignmentSpans.getOrElse(sym.span.start, Nil).exists(whileSpan.contains(_))
      && ctx.notNullInfos.impliesNotNull(ref)

    val retractedVars = ctx.notNullInfos.flatMap(_.asserted.filter(isRetracted)).toSet
    ctx.addNotNullInfo(NotNullInfo(Set(), retractedVars))
  end whileContext

  /** Post process all arguments to by-name parameters by removing any not-null
   *  info that was used when typing them. Concretely:
   *  If an argument corresponds to a call-by-name parameter, drop all
   *  embedded not-null assertions of the form `x.$asInstanceOf[x.type & T]`
   *  where `x` is a reference to a mutable variable. If the argument still typechecks
   *  with the removed assertions and is still compatible with the formal parameter,
   *  keep it. Otherwise issue an error that the call-by-name argument was typed using
   *  flow assumptions about mutable variables and suggest that it is enclosed
   *  in a `byName(...)` call instead.
   */
  def postProcessByNameArgs(fn: TermRef, app: Tree)(using Context): Tree =
    fn.widen match
      case mt: MethodType
      if mt.paramInfos.exists(_.isInstanceOf[ExprType]) && !fn.symbol.is(Inline) =>
        app match
          case Apply(fn, args) =>
            object dropNotNull extends TreeMap:
              var dropped: Boolean = false
              override def transform(t: Tree)(using Context) = t match
                case AssertNotNull(t0) if t0.symbol.is(Mutable) =>
                  nullables.println(i"dropping $t")
                  dropped = true
                  transform(t0)
                case t: ValDef if !t.symbol.is(Lazy) => super.transform(t)
                case t: MemberDef =>
                  // stop here since embedded references to mutable variables would be
                  // out of order, so they would not asserted ot be not-null anyway.
                  // @see Nullables.usedOutOfOrder
                  t
                case _ => super.transform(t)

            object retyper extends ReTyper:
              override def typedUnadapted(t: untpd.Tree, pt: Type, locked: TypeVars)(using Context): Tree = t match
                case t: untpd.ValDef if !t.symbol.is(Lazy) => super.typedUnadapted(t, pt, locked)
                case t: untpd.MemberDef => promote(t)
                case _ => super.typedUnadapted(t, pt, locked)

            def postProcess(formal: Type, arg: Tree): Tree =
              val nestedCtx = ctx.fresh.setNewTyperState()
              val arg1 = dropNotNull.transform(arg)(using nestedCtx)
              if !dropNotNull.dropped then arg
              else
                val arg2 = retyper.typed(arg1, formal)(using nestedCtx)
                if nestedCtx.reporter.hasErrors || !(arg2.tpe <:< formal) then
                  report.error(em"""This argument was typed using flow assumptions about mutable variables
                                |but it is passed to a by-name parameter where such flow assumptions are unsound.
                                |Wrapping the argument in `byName(...)` fixes the problem by disabling the flow assumptions.
                                |
                                |`byName` needs to be imported from the `scala.compiletime` package.""",
                            arg.srcPos)
                  arg
                else
                  nestedCtx.typerState.commit()
                  arg2

            def recur(formals: List[Type], args: List[Tree]): List[Tree] = (formals, args) match
              case (formal :: formalsRest, arg :: argsRest) =>
                val arg1 =
                  if formal.isInstanceOf[ExprType]
                  then postProcess(formal.widenExpr.repeatedToSingle, arg)
                  else arg
                val argsRest1 = recur(
                  if formal.isRepeatedParam then formals else formalsRest,
                  argsRest)
                if (arg1 eq arg) && (argsRest1 eq argsRest) then args
                else arg1 :: argsRest1
              case _ => args

            tpd.cpy.Apply(app)(fn, recur(mt.paramInfos, args))
          case _ => app
      case _ => app
  end postProcessByNameArgs
end Nullables
