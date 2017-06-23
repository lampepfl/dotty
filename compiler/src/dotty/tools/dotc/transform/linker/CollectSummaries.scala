package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.FromTasty.TASTYCompilationUnit
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core.tasty._
import dotty.tools.dotc.core.tasty.DottyUnpickler
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.transform.TreeGen
import dotty.tools.dotc.transform.TreeTransforms._
import dotty.tools.dotc.transform.linker.summaries._
import dotty.tools.dotc.transform.linker.types.{ClosureType, PreciseType}
import dotty.tools.dotc.typer.Applications
import dotty.tools.dotc.typer.Applications._

import scala.annotation.tailrec
import scala.collection.mutable

class CollectSummaries extends MiniPhase { thisTransform =>
  import tpd._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "summaries"

  val treeTransform: Collect = new Collect

  override def run(implicit ctx: Context): Unit = {
    if (CollectSummaries.isPhaseRequired)
      super.run
  }

  def methodSummaries: Map[Symbol, MethodSummary] = treeTransform.getMethodSummaries

  class Collect extends TreeTransform {
    def phase: CollectSummaries = thisTransform

    private var methodSummaries: Map[Symbol, MethodSummary] = Map.empty
    private var methodSummaryStack: mutable.Stack[MethodSummaryBuilder] = mutable.Stack()
    private var curMethodSummary: MethodSummaryBuilder = _

    def getMethodSummaries: Map[Symbol, MethodSummary] = methodSummaries

    override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
      if (ctx.compilationUnit.isInstanceOf[TASTYCompilationUnit])
        NoTransform // will retrieve them lazily
      else this
    }

    override def prepareForDefDef(tree: tpd.DefDef)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      if (!sym.is(Label) && !sym.isPrimaryConstructor) {
        methodSummaryStack.push(curMethodSummary)
        val args = tree.vparamss.flatten.map(_.symbol) // outer param for constructors
        val argumentStoredToHeap = (0 to args.length).map(_ => true).toList
        curMethodSummary = new MethodSummaryBuilder(sym, argumentStoredToHeap)
      }
      this
    }

    override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Label) && !tree.symbol.isPrimaryConstructor) {
        assert(curMethodSummary.methodDef eq tree.symbol)
        assert(!methodSummaries.contains(curMethodSummary.methodDef))
        methodSummaries = methodSummaries.updated(curMethodSummary.methodDef, curMethodSummary.result())
        curMethodSummary = methodSummaryStack.pop()
      }
      tree
    }

    override def prepareForValDef(tree: tpd.ValDef)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      if (sym.exists && ((sym.is(Lazy) &&  (sym.owner.is(Package) || sym.owner.isClass)) ||  //lazy vals and modules
          sym.owner.name.startsWith(StdNames.str.LOCALDUMMY_PREFIX) || // blocks inside constructor
          sym.owner.isClass)) { // fields
        // owner is a template
        methodSummaryStack.push(curMethodSummary)
        curMethodSummary = new MethodSummaryBuilder(sym, List(true))
      }
      this
    }

    override def transformValDef(tree: tpd.ValDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      if (sym.exists) {
        val ownerIsClass = sym.owner.isClass
        val isLazyValOrModule = sym.is(Lazy) && (ownerIsClass || sym.owner.is(Package))
        val isBockInsideConstructor = sym.owner.name.startsWith(StdNames.str.LOCALDUMMY_PREFIX)
        if (isLazyValOrModule || isBockInsideConstructor || ownerIsClass) {
          assert(curMethodSummary.methodDef eq tree.symbol)
          assert(!methodSummaries.contains(curMethodSummary.methodDef))
          methodSummaries = methodSummaries.updated(curMethodSummary.methodDef, curMethodSummary.result())
          curMethodSummary = methodSummaryStack.pop()
        }
        if (!isLazyValOrModule && (isBockInsideConstructor || ownerIsClass))
          registerCall(tree)
      }
      tree
    }

    override def prepareForTemplate(tree: tpd.Template)(implicit ctx: Context): TreeTransform = {
      val sym = tree.symbol
      assert(!sym.is(Label))
      methodSummaryStack.push(curMethodSummary)
      curMethodSummary = new MethodSummaryBuilder(sym.owner.primaryConstructor, List(true))
      this
    }

    override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      assert(!sym.is(Label))
      assert(curMethodSummary.methodDef eq tree.symbol.owner.primaryConstructor)
      assert(!methodSummaries.contains(curMethodSummary.methodDef))
      methodSummaries = methodSummaries.updated(curMethodSummary.methodDef, curMethodSummary.result())
      curMethodSummary = methodSummaryStack.pop()
      tree
    }

    /*
    override def transformTypeDef(tree: tpd.TypeDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      if (sym.isClass) {
        val isEntryPoint = dotty.tools.backend.jvm.CollectEntryPoints.isJavaEntryPoint(sym)
        /*summaries = ClassSummary(sym.asClass,
          methodSummaries
        ) :: summaries
        methodSummaries = Nil*/
      }
      tree
    }
    */

    def registerModule(sym: Symbol)(implicit ctx: Context): Unit = {
      if ((curMethodSummary ne null) && sym.is(ModuleVal)) {
        curMethodSummary.addAccessedModules(sym)
        registerModule(sym.owner)
      }
      val res = sym.info.finalResultType.termSymbol
      if ((curMethodSummary ne null) && res.is(ModuleVal)) {
        curMethodSummary.addAccessedModules(res)
        registerModule(res.owner)
      }

    }

    def registerCall(tree: Tree)(implicit ctx: Context): Unit = {

      def symbolOf(t: Tree) = {
        val s = t.symbol.orElse(t.tpe.classSymbol).orElse(TypeErasure.erasure(t.tpe).classSymbol)
        assert(s.exists)
        s
      }

      @tailrec def receiverArgumentsAndSymbol(t: Tree, accArgs: List[List[Tree]] = Nil, accT: List[Tree] = Nil):
          (Tree, Tree, List[List[Tree]], List[Tree], TermRef) = t match {
        case Block(stats, expr) => receiverArgumentsAndSymbol(expr, accArgs, accT)
        case TypeApply(fun, targs) if fun.symbol eq t.symbol => receiverArgumentsAndSymbol(fun, accArgs, targs)
        case Apply(fn, args) if fn.symbol == t.symbol => receiverArgumentsAndSymbol(fn, args :: accArgs, accT)
        case Select(qual, _) =>
          (qual, t, accArgs, accT, t.tpe.asInstanceOf[TermRef])
        case x: This => (x, x, accArgs, accT, x.tpe.asInstanceOf[TermRef])
        case x => (x, x, accArgs, accT, x.tpe.asInstanceOf[TermRef])
      }
      val widenedTp = tree.tpe.widen
      if (!widenedTp.isInstanceOf[MethodicType] || (tree.symbol.exists && !tree.symbol.info.isInstanceOf[MethodicType])) {
        val (receiver, _ /*call*/ , arguments, typeArguments, method) = receiverArgumentsAndSymbol(tree)

        val storedReceiver = receiver.tpe

        assert(storedReceiver.exists)

        def wrapArrayTermRef(wrapArrayMethodName: TermName) =
          TermRef(defn.ScalaPredefModuleRef, defn.ScalaPredefModule.requiredMethod(wrapArrayMethodName))

        @tailrec def skipBlocks(s: Tree): Tree = s match {
          case s: Block => skipBlocks(s.expr)
          case _ => s
        }

        @tailrec def argType(x: Tree): Type = skipBlocks(x) match {
          case exp: Closure =>
            val SAMType(e) = exp.tpe
            new ClosureType(exp, x.tpe, e.symbol)
          case Select(New(tp), _) => new PreciseType(tp.tpe)
          case Apply(Select(New(tp), _), args) => new PreciseType(tp.tpe)
          case Apply(TypeApply(Select(New(tp), _), targs), args) => new PreciseType(tp.tpe)
          case Typed(expr: SeqLiteral, tpt) if x.tpe.isRepeatedParam =>
            val tp = expr.elemtpt.tpe
            wrapArrayTermRef(TreeGen.wrapArrayMethodName(tp)).widenDealias match {
              case warr: PolyType => warr.appliedTo(tp).finalResultType
              case warr => warr.finalResultType
            }
          case Typed(expr, _) => argType(expr)
          case NamedArg(nm, a) => argType(a)
          case _ => x.tpe
        }

        val thisCallInfo = CallInfo(method, typeArguments.map(_.tpe), arguments.flatten.map(argType))
        lazy val someThisCallInfo = Some(thisCallInfo)

        // Create calls to wrapXArray for varArgs
        val repeatedArgsCalls = tree match {
          case Apply(fun, _) if fun.symbol.info.isVarArgsMethod =>
            @tailrec def refine(tp: Type): Type = tp match {
              case tp: TypeAlias => refine(tp.alias.dealias)
              case tp: RefinedType if tp.parent == defn.RepeatedParamType => refine(tp.refinedInfo)
              case tp: TypeBounds => refine(tp.hi)
              case _ => tp
            }
            @tailrec def getVarArgTypes(tp: Type, acc: List[Type] = Nil): List[Type] = tp match {
              case tp: PolyType => getVarArgTypes(tp.resultType, acc)
              case tp: MethodType =>
                val paramInfos = tp.paramInfos
                lazy val lastParamType = paramInfos.last
                if (paramInfos.isEmpty || !lastParamType.isRepeatedParam) acc
                else getVarArgTypes(tp.resultType, refine(lastParamType) :: acc)
              case _ => acc
            }

            getVarArgTypes(fun.tpe.widenDealias).map { tp =>
              val wrapArrayName = TreeGen.wrapArrayMethodName(tp)
              val targs = if (wrapArrayName == nme.wrapRefArray || wrapArrayName == nme.genericWrapArray) List(tp) else Nil
              val args = List(defn.ArrayOf(tp))
              CallInfo(wrapArrayTermRef(wrapArrayName), targs, args, someThisCallInfo)
            }

          case _ => Nil
        }

        val isInPredef =
          ctx.owner.ownersIterator.exists(owner => owner == defn.ScalaPredefModule || owner.companionModule == defn.ScalaPredefModule)

        val isMethodOnPredef = {
          val funTpe = tree match {
            case Apply(TypeApply(fun: Ident, _), _) => fun.tpe
            case Apply(fun: Ident, _) => fun.tpe
            case _ => tree.tpe
          }
          funTpe.normalizedPrefix == defn.ScalaPredefModuleRef
        }

        val loadPredefModule = if (!isInPredef && (repeatedArgsCalls.nonEmpty || isMethodOnPredef)) {
          List(CallInfo(defn.ScalaPredefModuleRef, Nil, Nil, someThisCallInfo))
        } else {
          Nil
        }

        val sym = tree.symbol
        val mixinConstructors: List[CallInfo] = {
          if (!sym.isPrimaryConstructor) {
            Nil
          } else {
            val directMixins = sym.owner.mixins.diff(sym.owner.info.parents.head.symbol.mixins)
            directMixins.collect {
              case mixin if !mixin.is(NoInits) =>
                val decl = mixin.primaryConstructor
                val (tparams, params) = decl.info match {
                  case tp: PolyType => (mixin.info.typeParams.map(_.paramRef), tp.resType.paramInfoss.flatten)
                  case tp => (Nil, tp.paramInfoss.flatten)
                }
                CallInfo(decl.termRef, tparams, params, someThisCallInfo)
            }
          }
        }

        val languageDefinedCalls = loadPredefModule ::: mixinConstructors ::: repeatedArgsCalls

        curMethodSummary.addMethodsCalledBy(storedReceiver, thisCallInfo :: languageDefinedCalls)
      }
    }

    override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Package)) {
        registerModule(tree.symbol)
      }
      val select = tree.tpe match {
        case TermRef(prefix: TermRef, name) =>
          Some(tpd.ref(prefix).select(tree.symbol))
        case TermRef(prefix: ThisType, name) =>
          Some(tpd.This(prefix.cls).select(tree.symbol))
        case TermRef(NoPrefix, name) =>
          if (tree.symbol is Method | Lazy) { // todo: this kills dotty {
            val widenedTp = tree.tpe.widen
            if (widenedTp.isInstanceOf[MethodicType] && (!tree.symbol.exists || tree.symbol.info.isInstanceOf[MethodicType]))
              return tree
            registerCall(tree)
            return tree
            // Some(This(tree.symbol.topLevelClass.asClass).select(tree.symbol)) // workaround #342 todo: remove after fixed
          }
          else None
        case _ => None
      }

      select.map(transformSelect)

      tree
    }

    override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      val sym = tree.symbol
      if (!sym.is(Package | Label) && !sym.isClass && !sym.isType) {
        registerModule(sym)
        registerCall(tree)
      }
      // handle nullary methods
      tree
    }

    override def transformThis(tree: tpd.This)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      curMethodSummary.setThisAccessed(true)
      tree
    }

    override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (!tree.symbol.is(Label))
        registerCall(tree)
      tree
    }

    override def transformTypeApply(tree: tpd.TypeApply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      registerCall(tree)
      tree
    }

    def registerUnApply(selector: tpd.Tree, tree: tpd.UnApply)(implicit ctx: Context, info: TransformerInfo): Unit = {
      def registerNestedUnapply(nestedSelector: Tree, nestedPattern: Tree): Unit = nestedPattern match {
        case nestedUnapply: UnApply => registerUnApply(nestedSelector, nestedUnapply)
        case _ =>
      }

      def registerNestedUnapplyFromProduct(product: Tree, patterns: List[Tree]): Unit =
        for ((nestedPat, idx) <- patterns.zipWithIndex) {
          val nestedSel = product.select(nme.selectorName(idx))
          registerCall(nestedSel) // register call to Product._x
          registerNestedUnapply(nestedSel, nestedPat)
        }

      def registerNestedUnapplyFromSeq(seq: Tree, patterns: List[Tree]): Unit = {
        registerCall(seq.select(nme.lengthCompare).appliedTo(Literal(Constant(patterns.size))))

        if (patterns.size >= 1) {
          val headSel  = seq.select(nme.head)
          val tailSels = for (i <- 1 until patterns.size) yield seq.select(nme.apply).appliedTo(Literal(Constant(i)))
          val nestedSels = Seq(headSel) ++ tailSels

          for ((nestedSel, nestedPat) <- nestedSels zip patterns) {
            registerCall(nestedSel)
            registerNestedUnapply(nestedSel, nestedPat)
          }
        }
      }

      val unapplyCall = Apply(tree.fun, List(selector))
      registerCall(unapplyCall)

      val unapplyResultType = unapplyCall.tpe
      val hasIsDefined = extractorMemberType(unapplyResultType, nme.isEmpty) isRef defn.BooleanClass
      val hasGet = extractorMemberType(unapplyResultType, nme.get).exists

      if (hasIsDefined && hasGet) { // if result of unapply is an Option
        val getCall = unapplyCall.select(nme.get)

        // register Option.isDefined and Option.get calls
        registerCall(unapplyCall.select(nme.isEmpty))
        registerCall(getCall)

        if (tree.fun.symbol.name == nme.unapplySeq)                 // result of unapplySeq is Option[Seq[T]]
          registerNestedUnapplyFromSeq(getCall, tree.patterns)
        else if (tree.patterns.size == 1)                           // result of unapply is Option[T]
          registerNestedUnapply(getCall, tree.patterns.head)
        else                                                        // result of unapply is Option[(T1, ..., Tn)]
          registerNestedUnapplyFromProduct(getCall, tree.patterns)

      } else if (defn.isProductSubType(unapplyResultType)) {
        // if result of unapply is a Product
        registerNestedUnapplyFromProduct(unapplyCall, tree.patterns)
      }
    }

    def collectMatch(selector: tpd.Tree, cases: List[tpd.CaseDef])(implicit ctx: Context, info: TransformerInfo): Unit = {
      cases foreach { case CaseDef(pat, _, _) => pat match {
        case unapply: tpd.UnApply => registerUnApply(selector, unapply)
        case _ =>
      }}
    }

    override def transformMatch(tree: tpd.Match)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      collectMatch(tree.selector, tree.cases)

      tree
    }

    override def transformTry(tree: tpd.Try)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      // generate synthetic selector of Throwable type (from TryCatchPatterns.scala)
      val exName = NameKinds.DefaultExceptionName.fresh()
      val fallbackSelector = ctx.newSymbol(ctx.owner, exName, Flags.Synthetic | Flags.Case, defn.ThrowableType)
      val sel = Ident(fallbackSelector.termRef)

      collectMatch(sel, tree.cases)

      tree
    }


    override def transformClosure(tree: tpd.Closure)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      if (curMethodSummary ne null) {
        curMethodSummary.addDefinedClosure(new ClosureType(tree, tree.tpe, tree.meth.symbol))
      }
      tree
    }

    override def transformUnit(tree: tpd.Tree)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
      TastySummaries.saveInTasty(methodSummaries.values.toList)
      tree
    }
  }
}

object CollectSummaries {

  def isPhaseRequired(implicit ctx: Context): Boolean = BuildCallGraph.isPhaseRequired

}
