package dotty.tools
package dotc
package semanticdb

import core.Symbols._
import core.Contexts.Context
import core.Types._
import core.Annotations.Annotation
import core.Flags
import core.Names.Name
import core.StdNames.tpnme
import core.Decorators.em
import scala.util.chaining.scalaUtilChainingOps

import collection.mutable

import dotty.tools.dotc.{semanticdb => s}
import Scala3.{FakeSymbol, SemanticSymbol, WildcardTypeSymbol, TypeParamRefSymbol, TermParamRefSymbol, RefinementSymbol}
import dotty.tools.dotc.core.Names.Designator

class TypeOps:
  import SymbolScopeOps._
  import Scala3.given
  private val paramRefSymtab = mutable.Map[(LambdaType, Name), Symbol]()
  private val refinementSymtab = mutable.Map[(RefinedType, Name), Symbol]()

  // save generated fake symbols so we can insert them into symbols section of SemanticDB
  val fakeSymbols = mutable.Set[FakeSymbol]()
  given typeOps: TypeOps = this

  extension [T <: LambdaType | RefinedType](symtab: mutable.Map[(T, Name), Symbol])
    private def lookup(
      binder: T,
      name: Name,
    )(using Context): Option[Symbol] =
      symtab.get((binder, name))

  extension [T <: LambdaType | RefinedType](symtab: mutable.Map[(T, Name), Symbol])
    private def lookupOrErr(
      binder: T,
      name: Name,
      parent: Symbol,
    )(using Context): Option[Symbol] =
      // In case refinement or type param cannot be accessed from traverser and
      // no symbols are registered to the symbol table, fall back to Type.member
      symtab.lookup(binder, name) match
        case found @ Some(_) => found
        case None =>
          val member = binder.member(name).symbol
          if !member.exists then
            symbolNotFound(binder, name, parent)
            None
          else
            Some(member)

  private def symbolNotFound(binder: Type, name: Name, parent: Symbol)(using ctx: Context): Unit =
    warn(s"Ignoring ${name} of symbol ${parent}, type ${binder}")
  private def symbolNotFound(name: Name, parent: Symbol)(using ctx: Context): Unit =
    warn(s"Ignoring ${name} of symbol ${parent}")

  private def warn(msg: String)(using ctx: Context): Unit =
    report.warning(
      em"Internal error in extracting SemanticDB while compiling ${ctx.compilationUnit.source}: ${msg}"
    )

  private def registerFakeSymbol(sym: FakeSymbol)(using Context, SemanticSymbolBuilder): Unit =
    fakeSymbols.add(sym)

  extension (tpe: Type)
    def lookupSym(name: Name)(using Context): Option[Symbol] = {
      def loop(ty: Type): Option[Symbol] = ty match
        case rt: RefinedType =>
          refinementSymtab.lookup(rt, name).orElse(
            loop(rt.parent)
          )
        case rec: RecType =>
          loop(rec.parent)
        case AndType(tp1, tp2) =>
          loop(tp1).orElse(loop(tp2))
        case OrType(tp1, tp2) =>
          loop(tp1).orElse(loop(tp2))
        case _ =>
          None
      loop(tpe.dealias)
    }

    def toSemanticSig(using LinkMode, Context, SemanticSymbolBuilder)(sym: Symbol): s.Signature =
      def enterParamRef(tpe: Type): Unit =
        tpe match {
          case lam: LambdaType =>
            // Find the "actual" binder type for nested LambdaType
            // For example, `def foo(x: T)(y: T): T` and for `<y>.owner.info` would be like
            // `MethodType(...<x>, resType = MethodType(...<y>, resType = <T>))`.
            // (Let's say the outer `MethodType` "outer", and `MethodType` who is
            // `resType` of outer "inner")
            //
            // We try to find the "actual" binder of <y>: `inner`,
            // and register them to the symbol table with `(<y>, inner) -> <y>`
            // instead of `("y", outer) -> <y>`
            if lam.paramNames.contains(sym.name) then
              paramRefSymtab((lam, sym.name)) = sym
            else
              enterParamRef(lam.resType)

          // for CaseType `case Array[t] => t` which is represented as [t] =>> MatchCase[Array[t], t]
          case m: MatchType =>
            m.cases.foreach(enterParamRef)

          // for class constructor
          // class C[T] { ... }
          case cls: ClassInfo if sym.info.isInstanceOf[LambdaType] =>
            val lam = sym.info.asInstanceOf[LambdaType]
            cls.cls.typeParams.foreach { param =>
              paramRefSymtab((lam, param.name)) = param
            }

          // type X[T] = ...
          case tb: TypeBounds =>
            enterParamRef(tb.lo)
            enterParamRef(tb.hi)

          case _ => ()
        }

      def enterRefined(tpe: Type): Unit =
        tpe match {
          case refined: RefinedType =>
            val key = (refined, sym.name)
            refinementSymtab(key) = sym

          case rec: RecType =>
            enterRefined(rec.parent)

          // Register symbol for opaque type,
          // opaque type alias will be stored into the refinement of
          // the self type of the enclosing class.
          // Key: the tuple of
          //   - self-type of enclosing class
          //   - name of the opaque type
          // Value: the symbol of the opaque type
          // See: SymDenotation.opaqueToBounds
          case cls: ClassInfo if sym.is(Flags.Opaque) =>
            cls.classSymbol.asClass.givenSelfType match
              case rt: RefinedType =>
                refinementSymtab((rt, sym.name)) = sym
              case _ => ()

          case cls: ClassInfo if (cls.cls.name == tpnme.REFINE_CLASS) =>
            enterRefined(sym.owner.owner.info)

          // type x = Person { refinement }
          case tb: TypeBounds =>
            // tb = TypeBounds(
            //   lo = RefinedType(...)
            //   hi = RefinedType(...)
            // )
            enterRefined(tb.lo)
            enterRefined(tb.hi)

          // def s(x: Int): { refinement } = ...
          case expr: ExprType =>
            enterRefined(expr.resType)
          case m: LambdaType =>
            enterRefined(m.resType)
          case AndType(t1, t2) =>
            enterRefined(t1)
            enterRefined(t2)
          case OrType(t1, t2) =>
            enterRefined(t1)
            enterRefined(t2)
          case _ => ()
        }
      if sym.exists && sym.owner.exists then
        enterParamRef(sym.owner.info)
        enterRefined(sym.owner.info)

      def loop(tpe: Type): s.Signature = tpe match {
        case mp: MethodOrPoly =>
          def flatten(
            t: Type,
            paramss: List[List[SemanticSymbol]],
            tparams: List[SemanticSymbol]
          ): (Type, List[List[SemanticSymbol]], List[SemanticSymbol]) = t match {
            case mt: MethodType =>
              val syms: List[SemanticSymbol] = mt.paramNames.zip(mt.paramInfos).map { (name, info) =>
                paramRefSymtab.lookup(mt, name).getOrElse {
                  TermParamRefSymbol(sym, name, info).tap(registerFakeSymbol)
                }
              }
              flatten(mt.resType, paramss :+ syms, tparams)
            case pt: PolyType =>
              val syms: List[SemanticSymbol] = pt.paramNames.zip(pt.paramInfos).map { (name, info) =>
                paramRefSymtab.lookup(pt, name).getOrElse {
                  TypeParamRefSymbol(sym, name, info).tap(registerFakeSymbol)
                }
              }
              flatten(pt.resType, paramss, tparams ++ syms)
            case other =>
              (other, paramss, tparams)
          }
          val (resType, paramss, tparams) = flatten(mp, Nil, Nil)

          val sparamss = paramss.map(_.sscope)
          val stparams = tparams.sscopeOpt
          s.MethodSignature(
            stparams,
            sparamss,
            resType.toSemanticType(sym)
          )

        case cls: ClassInfo =>
          val stparams = cls.cls.typeParams.sscopeOpt
          val sparents = cls.parents.map(_.toSemanticType(sym))
          val sself = cls.selfType.toSemanticType(sym)
          val decls = cls.decls.toList.sscopeOpt
          s.ClassSignature(stparams, sparents, sself, decls)

        case TypeBounds(lo, hi) =>
          // for `type X[T] = T` is equivalent to `[T] =>> T`
          def tparams(tpe: Type): (Type, List[SemanticSymbol]) = tpe match {
            case lambda: HKTypeLambda =>
              val paramSyms: List[SemanticSymbol] = lambda.paramNames.zip(lambda.paramInfos).map { (paramName, bounds) =>
                // def x[T[_]] = ???
                if paramName.isWildcard then
                  WildcardTypeSymbol(sym, bounds).tap(registerFakeSymbol)
                else
                  paramRefSymtab.lookup(lambda, paramName).getOrElse {
                    TypeParamRefSymbol(sym, paramName, bounds).tap(registerFakeSymbol)
                  }
              }
              (lambda.resType, paramSyms)
            case _ => (tpe, Nil)
          }
          val (loRes, loParams) = tparams(lo)
          val (hiRes, hiParams) = tparams(hi)
          val stparams = (loParams ++ hiParams).distinctBy(_.name).sscopeOpt
          val slo = loRes.toSemanticType(sym)
          val shi = hiRes.toSemanticType(sym)
          s.TypeSignature(stparams, slo, shi)

        case other =>
          s.ValueSignature(
            other.toSemanticType(sym)
          )
      }
      loop(tpe)

    def toSemanticType(sym: Symbol)(using LinkMode, SemanticSymbolBuilder, Context): s.Type =
      import ConstantOps._
      def loop(tpe: Type): s.Type = tpe match {
        case t if t.isFromJavaObject =>
          loop(defn.AnyType)
        case ExprType(tpe) =>
          val stpe = loop(tpe)
          s.ByNameType(stpe)

        // sym of `TypeRef(_, sym)` may not be a Symbol but Name in some cases
        // e.g. in MatchType,
        // case x *: xs => x *: Concat[xs, Ys]
        // x and xs should have a typebounds <: Any, >: Nothing
        // but Any (and Nothing) are represented as TypeRef(<scala>, "Any" <- Name)
        case tr @ TypeRef(pre, _) if tr.symbol != NoSymbol =>
          val spre = if tpe.hasTrivialPrefix then s.Type.Empty else loop(pre)
          val ssym = tr.symbol.symbolName
          s.TypeRef(spre, ssym, Seq.empty)

        // when TypeRef refers the refinement of RefinedType e.g.
        // TypeRef for `foo.B` in `trait T[A] { val foo: { type B = A } = ???; def bar(b: foo.B) = () }` has NoSymbol
        case TypeRef(pre, name: Name) =>
          val spre = if tpe.hasTrivialPrefix then s.Type.Empty else loop(pre)
          val maybeSym = pre.widen.dealias.lookupSym(name)
          maybeSym match
            case Some(sym) =>
              s.TypeRef(spre, sym.symbolName, Seq.empty)
            case None => s.Type.Empty

        case tr @ TermRef(pre, _) if tr.symbol != NoSymbol =>
          val spre = if(tpe.hasTrivialPrefix) s.Type.Empty else loop(pre)
          val ssym = tr.symbol.symbolName
          s.SingleType(spre, ssym)

        case TermRef(pre, name: Name) =>
          val spre = if tpe.hasTrivialPrefix then s.Type.Empty else loop(pre)
          val maybeSym = pre.widen.dealias match
            case rt: RefinedType =>
              refinementSymtab.lookupOrErr(rt, name, rt.typeSymbol)
            case _ => None
          maybeSym match
            case Some(sym) =>
              s.SingleType(spre, sym.symbolName)
            case None => s.Type.Empty

        case ThisType(TypeRef(_, sym: Symbol)) =>
          s.ThisType(sym.symbolName)

        case tref: TermParamRef =>
          paramRefSymtab.lookupOrErr(
            tref.binder, tref.paramName, sym
          ) match
            case Some(ref) =>
              val ssym = ref.symbolName
              s.SingleType(s.Type.Empty, ssym)
            case None =>
              s.Type.Empty

        case tref: TypeParamRef =>
          val tsym = paramRefSymtab.lookup(tref.binder, tref.paramName) match
            case found @ Some(sym) => found
            case None =>
              tref.binder.typeParams.find(param => param.paramName == tref.paramName) match
                case Some(param) =>
                  val info = param.paramInfo
                  Some(TypeParamRefSymbol(sym, tref.paramName, info).tap(registerFakeSymbol))
                case None =>
                  symbolNotFound(tref.binder, tref.paramName, sym)
                  None
          tsym match
            case Some(sym) =>
              val ssym = sym.symbolName
              s.TypeRef(s.Type.Empty, ssym, Seq.empty)
            case None =>
              s.Type.Empty

        case SuperType(thistpe, supertpe) =>
          val spre = loop(thistpe.typeSymbol.info)
          val ssym = supertpe.typeSymbol.symbolName
          s.SuperType(spre, ssym)

        // val clazzOf = classOf[...]
        case ConstantType(const) if const.tag == core.Constants.ClazzTag =>
          loop(const.typeValue)

        case ConstantType(const) =>
          s.ConstantType(const.toSemanticConst)

        case matchType: MatchType =>
          val scases = matchType.cases.map { caseType => caseType match {
            case lam: HKTypeLambda => // case Array[t] => t
              val paramSyms = lam.paramNames.flatMap { paramName =>
                val key = (lam, paramName)
                paramRefSymtab.get(key)
              }.sscope
              lam.resType match {
                case defn.MatchCase(key, body) =>
                  s.MatchType.CaseType(
                    loop(key),
                    loop(body)
                  )
                case _ => s.MatchType.CaseType() // shouldn't happen
              }
            case defn.MatchCase(key, body) =>
              val skey = loop(key)
              val sbody = loop(body)
              s.MatchType.CaseType(skey, sbody)
            case _ => s.MatchType.CaseType() // shouldn't happen
          }}
          val sscrutinee = loop(matchType.scrutinee)
          val sbound = loop(matchType.bound)
          s.MatchType(sscrutinee, scases)

        case rt @ RefinedType(parent, name, info) =>
          // `X { def x: Int; def y: Int }`
          // RefinedType(
          //   parent = RefinedType(
          //     parent = TypeRef(..., X)
          //     ...
          //   )
          //   refinedName = x
          //   refinedInfo = TypeRef(..., Int)
          // )
          type RefinedInfo = (core.Names.Name, Type)
          def flatten(tpe: Type, acc: List[RefinedInfo]): (Type, List[RefinedInfo]) = tpe match {
            case RefinedType(parent, name, info) =>
              flatten(parent, acc :+ (name, info))
            case _ =>
              (tpe, acc)
          }

          // flatten parent types to list
          // e.g. `X with Y with Z { refined }`
          // RefinedType(parent = AndType(X, AndType(Y, Z)), ...)
          // => List(X, Y, Z)
          def flattenParent(parent: Type): List[s.Type] = parent match {
            case AndType(tp1, tp2) =>
              flattenParent(tp1) ++ flattenParent(tp2)
            case _ => List(loop(parent))
          }

          val (parent, refinedInfos) = flatten(rt, List.empty)
          val stpe = s.IntersectionType(flattenParent(parent))

          val decls: List[SemanticSymbol] = refinedInfos.map { (name, info) =>
            refinementSymtab.lookup(rt, name).getOrElse {
              RefinementSymbol(sym, name, info).tap(registerFakeSymbol)
            }
          }
          val sdecls = decls.sscopeOpt(using LinkMode.HardlinkChildren)
          s.StructuralType(stpe, sdecls)

        case rec: RecType =>
          loop(rec.parent) // should be handled as RefinedType

        // repeated params: e.g. `Int*`, which is the syntax sugar of
        // `Seq[Int] @Repeated` (or `Array[Int] @Repeated`)
        // See: Desugar.scala and TypeApplications.scala
        case AnnotatedType(AppliedType(_, targs), annot)
          if (annot matches defn.RepeatedAnnot) && (targs.length == 1) =>
          val stpe = loop(targs(0))
          s.RepeatedType(stpe)

        case ann: AnnotatedType if ann.annot.symbol.info.isInstanceOf[ClassInfo] =>
          def flatten(tpe: Type, annots: List[Annotation]): (Type, List[Annotation]) = tpe match
            case AnnotatedType(parent, annot) if annot.symbol.info.isInstanceOf[ClassInfo] =>
              flatten(parent, annot +: annots)
            case other => (other, annots)

          val (parent, annots) = flatten(ann, List.empty)
          val sparent = loop(parent)
          val sannots = annots.map(a =>
            s.Annotation(loop(a.symbol.info.asInstanceOf[ClassInfo].selfType))
          )
          s.AnnotatedType(sannots, sparent)

        case AppliedType(tycon, args) if tycon == defn.RepeatedParamType && args.length == 1 =>
          val stpe = loop(args(0))
          s.RepeatedType(stpe)

        case app @ AppliedType(tycon, args) =>
          val targs = args.map { arg =>
            arg match
              // For wildcard type C[_ <: T], it's internal type representation will be
              // `AppliedType(TypeBounds(lo = <Nothing>, hi = <T>))`.
              //
              // As scalameta for Scala2 does, we'll convert the wildcard type to
              // `ExistentialType(TypeRef(NoPrefix, C, <local0>), Scope(hardlinks = List(<local0>)))`
              // where `<local0>` has
              // display_name: "_" and,
              // signature: type_signature(..., lo = <Nothing>, hi = <T>)
              case bounds: TypeBounds =>
                val wildcardSym = WildcardTypeSymbol(sym, bounds)
                // DO NOT register the wildcard symbol to symbol section here
                // since it's not a symbol definition
                // registerFakeSymbol(wildcardSym)
                val ssym = wildcardSym.symbolName
                (Some(wildcardSym), s.TypeRef(s.Type.Empty, ssym, Seq.empty))
              case other =>
                val sarg = loop(other)
                (None, sarg)
          }
          val wildcardSyms = targs.flatMap(_._1)
          val sargs = targs.map(_._2)

          val applied = loop(tycon) match
            case ref @ s.TypeRef(_, _, targs) =>
              // For curried applied type `F[T][U]` and tycon is also an `AppliedType`
              // Convert it to TypeRef(..., targs = List(T, U))
              ref.copy(typeArguments = targs ++ sargs)
            case _ =>
              s.Type.Empty

          if (wildcardSyms.isEmpty) applied
          else s.ExistentialType(
            applied,
            wildcardSyms.sscopeOpt(using LinkMode.HardlinkChildren)
          )

        case and: AndType =>
          def flatten(child: Type): List[Type] = child match
            case AndType(ct1, ct2) => flatten(ct1) ++ flatten(ct2)
            case other => List(other)
          val stpes = flatten(and).map(loop)
          s.IntersectionType(stpes)

        case or: OrType =>
          def flatten(child: Type): List[Type] = child match
            case OrType(ct1, ct2) => flatten(ct1) ++ flatten(ct2)
            case other => List(other)
          val stpes = flatten(or).map(loop)
          s.UnionType(stpes)

        case l: LazyRef =>
          loop(l.ref)

        case NoPrefix =>
          s.Type.Empty

        // Not yet supported
        case _: HKTypeLambda =>
          s.Type.Empty

        case tvar: TypeVar =>
          loop(tvar.stripped)

        case _ =>
          s.Type.Empty
      }
      loop(tpe)

    /** Return true if the prefix is like `_root_.this` */
    private def hasTrivialPrefix(using Context): Boolean =
      def checkTrivialPrefix(pre: Type, sym: Symbol)(using Context): Boolean =
        pre =:= sym.owner.thisType
      tpe match {
        case TypeRef(pre, sym: Symbol) =>
          checkTrivialPrefix(pre, sym)
        case tr @ TypeRef(pre, _) if tr.symbol != NoSymbol =>
          checkTrivialPrefix(pre, tr.symbol)
        case TermRef(pre, sym: Symbol) =>
          checkTrivialPrefix(pre, sym)
        case tr @ TermRef(pre, _) if tr.symbol != NoSymbol =>
          checkTrivialPrefix(pre, tr.symbol)
        case _ => false
      }


object SymbolScopeOps:
  import Scala3.{_, given}
  extension (syms: List[SemanticSymbol])
    def sscope(using linkMode: LinkMode)(using SemanticSymbolBuilder, TypeOps, Context): s.Scope =
      linkMode match
        case LinkMode.SymlinkChildren =>
          s.Scope(symlinks = syms.map(_.symbolName))
        case LinkMode.HardlinkChildren =>
          s.Scope(hardlinks = syms.map(_.symbolInfo(Set.empty)))

    def sscopeOpt(using LinkMode, SemanticSymbolBuilder, TypeOps, Context): Option[s.Scope] =
      if syms.nonEmpty then Some(syms.sscope) else None
