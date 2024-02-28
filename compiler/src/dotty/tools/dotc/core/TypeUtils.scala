package dotty.tools
package dotc
package core

import TypeErasure.ErasedValueType
import Types.*, Contexts.*, Symbols.*, Flags.*, Decorators.*
import Names.{Name, TermName}
import Constants.Constant

class TypeUtils {

  /** A decorator that provides methods on types
   *  that are needed in the transformer pipeline.
   */
  extension (self: Type) {

    def isErasedValueType(using Context): Boolean =
      self.isInstanceOf[ErasedValueType]

    def isPrimitiveValueType(using Context): Boolean =
      self.classSymbol.isPrimitiveValueClass

    def isErasedClass(using Context): Boolean =
      self.underlyingClassRef(refinementOK = true).typeSymbol.is(Flags.Erased)

    /** Is this type a checked exception? This is the case if the type
     *  derives from Exception but not from RuntimeException. According to
     *  that definition Throwable is unchecked. That makes sense since you should
     *  neither throw nor catch `Throwable` anyway, so we should not define
     *  a capability to do so.
     */
    def isCheckedException(using Context): Boolean =
      self.derivesFrom(defn.ExceptionClass)
      && !self.derivesFrom(defn.RuntimeExceptionClass)

    def isByName: Boolean =
      self.isInstanceOf[ExprType]

    def ensureMethodic(using Context): Type = self match {
      case self: MethodicType => self
      case _ => if (ctx.erasedTypes) MethodType(Nil, self) else ExprType(self)
    }

    def widenToParents(using Context): Type = self.parents match {
      case Nil => self
      case ps => ps.reduceLeft(AndType(_, _))
    }

    /** The element types of this tuple type, which can be made up of EmptyTuple, TupleX and `*:` pairs
     */
    def tupleElementTypes(using Context): Option[List[Type]] =
      tupleElementTypesUpTo(Int.MaxValue)

    /** The element types of this tuple type, which can be made up of EmptyTuple, TupleX and `*:` pairs
     *  @param bound     The maximum number of elements that needs generating minus 1
     *                   The generation will stop once more than bound elems have been generated
     *  @param normalize If true, normalize and dealias at each step.
     *                   If false, never normalize and dealias only to find *:
     *                   and EmptyTuple types. This is useful for printing.
     */
    def tupleElementTypesUpTo(bound: Int, normalize: Boolean = true)(using Context): Option[List[Type]] =
      def recur(tp: Type, bound: Int): Option[List[Type]] =
        if bound < 0 then Some(Nil)
        else (if normalize then tp.normalized else tp).dealias match
          case AppliedType(tycon, hd :: tl :: Nil) if tycon.isRef(defn.PairClass) =>
            recur(tl, bound - 1).map(hd :: _)
          case tp: AppliedType if defn.isTupleNType(tp) && normalize =>
            Some(tp.args)  // if normalize is set, use the dealiased tuple
                           // otherwise rely on the default case below to print unaliased tuples.
          case tp: SkolemType =>
            recur(tp.underlying, bound)
          case tp: SingletonType =>
            if tp.termSymbol == defn.EmptyTupleModule then Some(Nil)
            else if normalize then recur(tp.widen, bound)
            else None
          case _ =>
            if defn.isTupleClass(tp.typeSymbol) && !normalize then Some(tp.dealias.argInfos)
            else None
      recur(self.stripTypeVar, bound)

    /** Is this a generic tuple but not already an instance of one of Tuple1..22? */
    def isGenericTuple(using Context): Boolean =
      self.derivesFrom(defn.PairClass)
      && !defn.isTupleNType(self.widenDealias)

    /** Is this a generic tuple that would fit into the range 1..22?
     *  In this case we need to cast it to make the TupleN members accessible.
     *  This works only for generic tuples of known size up to 22.
     */
    def isSmallGenericTuple(using Context): Boolean = genericTupleArityCompare < 0

    /** Is this a generic tuple with an arity above 22? */
    def isLargeGenericTuple(using Context): Boolean = genericTupleArityCompare > 0

    /** If this is a generic tuple with element types, compare the arity and return:
     *    * -1, if the generic tuple is small (<= MaxTupleArity)
     *    * 1, if the generic tuple is large (> MaxTupleArity)
     *    * 0 if this isn't a generic tuple with element types
     */
    def genericTupleArityCompare(using Context): Int =
      if self.isGenericTuple then
        self.widenTermRefExpr.tupleElementTypesUpTo(Definitions.MaxTupleArity).match
          case Some(elems) => if elems.length <= Definitions.MaxTupleArity then -1 else 1
          case _ => 0
      else 0

    /** Is this a large generic tuple and is `pat` TupleXXL?
     *  TupleXXL.unapplySeq extracts values of type TupleXXL
     *  but large scrutinee terms are typed as large generic tuples.
     *  This allows them to hold on to their precise element types,
     *  but it means type-wise, the terms don't conform to the
     *  extractor's parameter type, so this method identifies case.
     */
    def isTupleXXLExtract(pat: Type)(using Context): Boolean =
      pat.typeSymbol == defn.TupleXXLClass && self.isLargeGenericTuple

    /** The `*:` equivalent of an instance of a Tuple class */
    def toNestedPairs(using Context): Type =
      tupleElementTypes match
        case Some(types) => TypeOps.nestedPairs(types)
        case None => throw new AssertionError("not a tuple")

    def namedTupleElementTypesUpTo(bound: Int, normalize: Boolean = true)(using Context): List[(TermName, Type)] =
      (if normalize then self.normalized else self).dealias match
        case defn.NamedTuple(nmes, vals) =>
          val names = nmes.tupleElementTypesUpTo(bound, normalize).getOrElse(Nil).map:
            case ConstantType(Constant(str: String)) => str.toTermName
            case t => throw TypeError(em"Malformed NamedTuple: names must be string types, but $t was found.")
          val values = vals.tupleElementTypesUpTo(bound, normalize).getOrElse(Nil)
          names.zip(values)
        case t =>
          Nil

    def namedTupleElementTypes(using Context): List[(TermName, Type)] =
      namedTupleElementTypesUpTo(Int.MaxValue)

    def isNamedTupleType(using Context): Boolean = self match
      case defn.NamedTuple(_, _) => true
      case _ => false

    /** Drop all named elements in tuple type */
    def stripNamedTuple(using Context): Type = self.normalized.dealias match
      case defn.NamedTuple(_, vals) =>
        vals
      case self @ AnnotatedType(tp, annot) =>
        val tp1 = tp.stripNamedTuple
        if tp1 ne tp then AnnotatedType(tp1, annot) else self
      case _ =>
        self

    def refinedWith(name: Name, info: Type)(using Context) = RefinedType(self, name, info)

    /** The TermRef referring to the companion of the underlying class reference
     *  of this type, while keeping the same prefix.
     */
    def mirrorCompanionRef(using Context): TermRef = self match {
      case AndType(tp1, tp2) =>
        val c1 = tp1.classSymbol
        val c2 = tp2.classSymbol
        if c1.isSubClass(c2) then tp1.mirrorCompanionRef
        else tp2.mirrorCompanionRef // precondition: the parts of the AndType have already been checked to be non-overlapping
      case self @ TypeRef(prefix, _) if self.symbol.isClass =>
        prefix.select(self.symbol.companionModule).asInstanceOf[TermRef]
      case self: TypeProxy =>
        self.superType.mirrorCompanionRef
    }

    /** Is this type a methodic type that takes at least one parameter? */
    def takesParams(using Context): Boolean = self.stripPoly match
      case mt: MethodType => mt.paramNames.nonEmpty || mt.resType.takesParams
      case _ => false

    /** Is this type a methodic type that takes implicit parameters (both old and new) at some point? */
    def takesImplicitParams(using Context): Boolean = self.stripPoly match
      case mt: MethodType => mt.isImplicitMethod || mt.resType.takesImplicitParams
      case _ => false

    /** Is this a type deriving only from transparent classes?
     *  @param traitOnly  if true, all class symbols must be transparent traits
     */
    def isTransparent(traitOnly: Boolean = false)(using Context): Boolean = self match
      case AndType(tp1, tp2) =>
        tp1.isTransparent(traitOnly) && tp2.isTransparent(traitOnly)
      case _ =>
        val cls = self.underlyingClassRef(refinementOK = false).typeSymbol
        cls.isTransparentClass && (!traitOnly || cls.is(Trait))
  }
}
