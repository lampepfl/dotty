package dotty.tools.dotc
package ast

import core.*
import Symbols.*, Types.*, Contexts.*, Decorators.*, util.Spans.*, Flags.*, Constants.*
import StdNames.{nme, tpnme}
import ast.Trees.*
import Names.Name
import Comments.Comment
import NameKinds.DefaultGetterName
import Annotations.Annotation

object MainProxies {

  /** Generate proxy classes for @main functions.
   *  A function like
   *
   *     @main def f(x: S, ys: T*) = ...
   *
   *  would be translated to something like
   *
   *     import CommandLineParser.*
   *     class f {
   *       @static def main(args: Array[String]): Unit =
   *         try
   *           f(
   *             parseArgument[S](args, 0),
   *             parseRemainingArguments[T](args, 1): _*
   *           )
   *         catch case err: ParseError => showError(err)
   *       }
   */
  def proxies(stats: List[tpd.Tree])(using Context): List[untpd.Tree] = {
    import tpd.*
    def mainMethods(stats: List[Tree]): List[Symbol] = stats.flatMap {
      case stat: DefDef if stat.symbol.hasAnnotation(defn.MainAnnot) =>
        stat.symbol :: Nil
      case stat @ TypeDef(name, impl: Template) if stat.symbol.is(Module) =>
        mainMethods(impl.body)
      case _ =>
        Nil
    }
    mainMethods(stats).flatMap(mainProxy)
  }

  import untpd.*
  private def mainProxy(mainFun: Symbol)(using Context): List[TypeDef] = {
    val mainAnnotSpan = mainFun.getAnnotation(defn.MainAnnot).get.tree.span
    def pos = mainFun.sourcePos
    val argsRef = Ident(nme.args)

    def addArgs(call: untpd.Tree, mt: MethodType, idx: Int): untpd.Tree =
      if (mt.isImplicitMethod) {
        report.error(em"@main method cannot have implicit parameters", pos)
        call
      }
      else {
        val args = mt.paramInfos.zipWithIndex map {
          (formal, n) =>
            val (parserSym, formalElem) =
              if (formal.isRepeatedParam) (defn.CLP_parseRemainingArguments, formal.argTypes.head)
              else (defn.CLP_parseArgument, formal)
            val arg = Apply(
              TypeApply(ref(parserSym.termRef), TypeTree(formalElem) :: Nil),
              argsRef :: Literal(Constant(idx + n)) :: Nil)
            if (formal.isRepeatedParam) repeated(arg) else arg
        }
        val call1 = Apply(call, args)
        mt.resType match {
          case restpe: MethodType =>
            if (mt.paramInfos.lastOption.getOrElse(NoType).isRepeatedParam)
              report.error(em"varargs parameter of @main method must come last", pos)
            addArgs(call1, restpe, idx + args.length)
          case _ =>
            call1
        }
      }

    var result: List[TypeDef] = Nil
    if (!mainFun.owner.isStaticOwner)
      report.error(em"@main method is not statically accessible", pos)
    else {
      var call = ref(mainFun.termRef)
      mainFun.info match {
        case _: ExprType =>
        case mt: MethodType =>
          call = addArgs(call, mt, 0)
        case _: PolyType =>
          report.error(em"@main method cannot have type parameters", pos)
        case _ =>
          report.error(em"@main can only annotate a method", pos)
      }
      val errVar = Ident(nme.error)
      val handler = CaseDef(
        Typed(errVar, TypeTree(defn.CLP_ParseError.typeRef)),
        EmptyTree,
        Apply(ref(defn.CLP_showError.termRef), errVar :: Nil))
      val body = Try(call, handler :: Nil, EmptyTree)
      val mainArg = ValDef(nme.args, TypeTree(defn.ArrayType.appliedTo(defn.StringType)), EmptyTree)
        .withFlags(Param)
      /** Replace typed `Ident`s that have been typed with a TypeSplice with the reference to the symbol.
       *  The annotations will be retype-checked in another scope that may not have the same imports.
       */
      def insertTypeSplices = new TreeMap {
          override def transform(tree: Tree)(using Context): Tree = tree match
            case tree: tpd.Ident @unchecked => TypedSplice(tree)
            case tree => super.transform(tree)
      }
      val annots = mainFun.annotations
        .filterNot(_.matches(defn.MainAnnot))
        .map(annot => insertTypeSplices.transform(annot.tree))
      val mainMeth = DefDef(nme.main, (mainArg :: Nil) :: Nil, TypeTree(defn.UnitType), body)
        .withFlags(JavaStatic | Synthetic)
        .withAnnotations(annots)
      val mainTempl = Template(emptyConstructor, Nil, Nil, EmptyValDef, mainMeth :: Nil)
      val mainCls = TypeDef(mainFun.name.toTypeName, mainTempl)
        .withFlags(Final | Invisible)

      if (!ctx.reporter.hasErrors)
        result = mainCls.withSpan(mainAnnotSpan.toSynthetic) :: Nil
    }
    result
  }

}
