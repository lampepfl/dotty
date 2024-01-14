package dotty.tools
package dotc
package core

import Types.*, Contexts.*, Symbols.*, Constants.*, Decorators.*
import config.Printers.typr
import reporting.trace
import StdNames.tpnme
import Flags.CaseClass
import TypeOps.nestedPairs

object TypeEval:

  def tryCompiletimeConstantFold(tp: AppliedType)(using Context): Type = tp.tycon match
    case tycon: TypeRef if defn.isCompiletimeAppliedType(tycon.symbol) =>

      extension (tp: Type) def fixForEvaluation: Type =
        tp.normalized.dealias match
          // enable operations for constant singleton terms. E.g.:
          // ```
          // final val one = 1
          // type Two = one.type + one.type
          // ```
          case tp: TypeProxy =>
            val tp1 = tp.superType
            if tp1.isStable then tp1.fixForEvaluation else tp
          case tp => tp

      def constValue(tp: Type): Option[Any] = tp.fixForEvaluation match
        case ConstantType(Constant(n)) => Some(n)
        case _ => None

      def boolValue(tp: Type): Option[Boolean] = tp.fixForEvaluation match
        case ConstantType(Constant(n: Boolean)) => Some(n)
        case _ => None

      def intValue(tp: Type): Option[Int] = tp.fixForEvaluation match
        case ConstantType(Constant(n: Int)) => Some(n)
        case _ => None

      def longValue(tp: Type): Option[Long] = tp.fixForEvaluation match
        case ConstantType(Constant(n: Long)) => Some(n)
        case _ => None

      def floatValue(tp: Type): Option[Float] = tp.fixForEvaluation match
        case ConstantType(Constant(n: Float)) => Some(n)
        case _ => None

      def doubleValue(tp: Type): Option[Double] = tp.fixForEvaluation match
        case ConstantType(Constant(n: Double)) => Some(n)
        case _ => None

      def stringValue(tp: Type): Option[String] = tp.fixForEvaluation match
        case ConstantType(Constant(n: String)) => Some(n)
        case _ => None

      // Returns Some(true) if the type is a constant.
      // Returns Some(false) if the type is not a constant.
      // Returns None if there is not enough information to determine if the type is a constant.
      // The type is a constant if it is a constant type or a type operation composition of constant types.
      // If we get a type reference for an argument, then the result is not yet known.
      def isConst(tp: Type): Option[Boolean] = tp.dealias match
        // known to be constant
        case ConstantType(_) => Some(true)
        // currently not a concrete known type
        case TypeRef(NoPrefix,_) => None
        // currently not a concrete known type
        case _: TypeParamRef => None
        // constant if the term is constant
        case t: TermRef =>
          if t.denot.symbol.flagsUNSAFE.is(Flags.Param) then
            // might be substituted later
            None
          else
            isConst(t.underlying)
        // an operation type => recursively check all argument compositions
        case applied: AppliedType if defn.isCompiletimeAppliedType(applied.typeSymbol) =>
          val argsConst = applied.args.map(isConst)
          if (argsConst.exists(_.isEmpty)) None
          else Some(argsConst.forall(_.get))
        // all other types are considered not to be constant
        case _ => Some(false)

      def expectArgsNum(expectedNum: Int): Unit =
      // We can use assert instead of a compiler type error because this error should not
      // occur since the type signature of the operation enforces the proper number of args.
        assert(tp.args.length == expectedNum, s"Type operation expects $expectedNum arguments but found ${tp.args.length}")

      def natValue(tp: Type): Option[Int] = intValue(tp).filter(n => n >= 0 && n < Int.MaxValue)

      // Runs the op and returns the result as a constant type.
      // If the op throws an exception, then this exception is converted into a type error.
      def runConstantOp(op: => Any): Type =
        val result =
          try op
          catch case e: Throwable =>
            throw TypeError(em"${e.getMessage.nn}")
        ConstantType(Constant(result))

      def fieldsOf: Option[Type] =
        expectArgsNum(1)
        val arg = tp.args.head
        val cls = arg.classSymbol
        if cls.is(CaseClass) then
          val fields = cls.caseAccessors
          val fieldLabels = fields.map: field =>
            ConstantType(Constant(field.name.toString))
          val fieldTypes = fields.map(arg.memberInfo)
          Some:
            defn.NamedTupleTypeRef.appliedTo:
              nestedPairs(fieldLabels) :: nestedPairs(fieldTypes) :: Nil
        else
          None

      def constantFold1[T](extractor: Type => Option[T], op: T => Any): Option[Type] =
        expectArgsNum(1)
        extractor(tp.args.head).map(a => runConstantOp(op(a)))

      def constantFold2[T](extractor: Type => Option[T], op: (T, T) => Any): Option[Type] =
        constantFold2AB(extractor, extractor, op)

      def constantFold2AB[TA, TB](extractorA: Type => Option[TA], extractorB: Type => Option[TB], op: (TA, TB) => Any): Option[Type] =
        expectArgsNum(2)
        for
          a <- extractorA(tp.args(0))
          b <- extractorB(tp.args(1))
        yield runConstantOp(op(a, b))

      def constantFold3[TA, TB, TC](
        extractorA: Type => Option[TA],
        extractorB: Type => Option[TB],
        extractorC: Type => Option[TC],
        op: (TA, TB, TC) => Any
      ): Option[Type] =
        expectArgsNum(3)
        for
          a <- extractorA(tp.args(0))
          b <- extractorB(tp.args(1))
          c <- extractorC(tp.args(2))
        yield runConstantOp(op(a, b, c))

      trace(i"compiletime constant fold $tp", typr, show = true) {
        val sym = tycon.symbol
        val name = sym.name
        val owner = sym.owner
        val constantType =
          if defn.isCompiletime_S(sym) then
            constantFold1(natValue, _ + 1)
          else if defn.isNamedTuple_From(sym) then
            fieldsOf
          else if owner == defn.CompiletimeOpsAnyModuleClass then name match
            case tpnme.Equals     => constantFold2(constValue, _ == _)
            case tpnme.NotEquals  => constantFold2(constValue, _ != _)
            case tpnme.ToString   => constantFold1(constValue, _.toString)
            case tpnme.IsConst    => isConst(tp.args.head).map(b => ConstantType(Constant(b)))
            case _ => None
          else if owner == defn.CompiletimeOpsIntModuleClass then name match
            case tpnme.Abs        => constantFold1(intValue, _.abs)
            case tpnme.Negate     => constantFold1(intValue, x => -x)
            // ToString is deprecated for ops.int, and moved to ops.any
            case tpnme.ToString   => constantFold1(intValue, _.toString)
            case tpnme.Plus       => constantFold2(intValue, _ + _)
            case tpnme.Minus      => constantFold2(intValue, _ - _)
            case tpnme.Times      => constantFold2(intValue, _ * _)
            case tpnme.Div        => constantFold2(intValue, _ / _)
            case tpnme.Mod        => constantFold2(intValue, _ % _)
            case tpnme.Lt         => constantFold2(intValue, _ < _)
            case tpnme.Gt         => constantFold2(intValue, _ > _)
            case tpnme.Ge         => constantFold2(intValue, _ >= _)
            case tpnme.Le         => constantFold2(intValue, _ <= _)
            case tpnme.Xor        => constantFold2(intValue, _ ^ _)
            case tpnme.BitwiseAnd => constantFold2(intValue, _ & _)
            case tpnme.BitwiseOr  => constantFold2(intValue, _ | _)
            case tpnme.ASR        => constantFold2(intValue, _ >> _)
            case tpnme.LSL        => constantFold2(intValue, _ << _)
            case tpnme.LSR        => constantFold2(intValue, _ >>> _)
            case tpnme.Min        => constantFold2(intValue, _ min _)
            case tpnme.Max        => constantFold2(intValue, _ max _)
            case tpnme.NumberOfLeadingZeros => constantFold1(intValue, Integer.numberOfLeadingZeros(_))
            case tpnme.ToLong     => constantFold1(intValue, _.toLong)
            case tpnme.ToFloat    => constantFold1(intValue, _.toFloat)
            case tpnme.ToDouble   => constantFold1(intValue, _.toDouble)
            case _ => None
          else if owner == defn.CompiletimeOpsLongModuleClass then name match
            case tpnme.Abs        => constantFold1(longValue, _.abs)
            case tpnme.Negate     => constantFold1(longValue, x => -x)
            case tpnme.Plus       => constantFold2(longValue, _ + _)
            case tpnme.Minus      => constantFold2(longValue, _ - _)
            case tpnme.Times      => constantFold2(longValue, _ * _)
            case tpnme.Div        => constantFold2(longValue, _ / _)
            case tpnme.Mod        => constantFold2(longValue, _ % _)
            case tpnme.Lt         => constantFold2(longValue, _ < _)
            case tpnme.Gt         => constantFold2(longValue, _ > _)
            case tpnme.Ge         => constantFold2(longValue, _ >= _)
            case tpnme.Le         => constantFold2(longValue, _ <= _)
            case tpnme.Xor        => constantFold2(longValue, _ ^ _)
            case tpnme.BitwiseAnd => constantFold2(longValue, _ & _)
            case tpnme.BitwiseOr  => constantFold2(longValue, _ | _)
            case tpnme.ASR        => constantFold2(longValue, _ >> _)
            case tpnme.LSL        => constantFold2(longValue, _ << _)
            case tpnme.LSR        => constantFold2(longValue, _ >>> _)
            case tpnme.Min        => constantFold2(longValue, _ min _)
            case tpnme.Max        => constantFold2(longValue, _ max _)
            case tpnme.NumberOfLeadingZeros =>
              constantFold1(longValue, java.lang.Long.numberOfLeadingZeros(_))
            case tpnme.ToInt      => constantFold1(longValue, _.toInt)
            case tpnme.ToFloat    => constantFold1(longValue, _.toFloat)
            case tpnme.ToDouble   => constantFold1(longValue, _.toDouble)
            case _ => None
          else if owner == defn.CompiletimeOpsFloatModuleClass then name match
            case tpnme.Abs        => constantFold1(floatValue, _.abs)
            case tpnme.Negate     => constantFold1(floatValue, x => -x)
            case tpnme.Plus       => constantFold2(floatValue, _ + _)
            case tpnme.Minus      => constantFold2(floatValue, _ - _)
            case tpnme.Times      => constantFold2(floatValue, _ * _)
            case tpnme.Div        => constantFold2(floatValue, _ / _)
            case tpnme.Mod        => constantFold2(floatValue, _ % _)
            case tpnme.Lt         => constantFold2(floatValue, _ < _)
            case tpnme.Gt         => constantFold2(floatValue, _ > _)
            case tpnme.Ge         => constantFold2(floatValue, _ >= _)
            case tpnme.Le         => constantFold2(floatValue, _ <= _)
            case tpnme.Min        => constantFold2(floatValue, _ min _)
            case tpnme.Max        => constantFold2(floatValue, _ max _)
            case tpnme.ToInt      => constantFold1(floatValue, _.toInt)
            case tpnme.ToLong     => constantFold1(floatValue, _.toLong)
            case tpnme.ToDouble   => constantFold1(floatValue, _.toDouble)
            case _ => None
          else if owner == defn.CompiletimeOpsDoubleModuleClass then name match
            case tpnme.Abs        => constantFold1(doubleValue, _.abs)
            case tpnme.Negate     => constantFold1(doubleValue, x => -x)
            case tpnme.Plus       => constantFold2(doubleValue, _ + _)
            case tpnme.Minus      => constantFold2(doubleValue, _ - _)
            case tpnme.Times      => constantFold2(doubleValue, _ * _)
            case tpnme.Div        => constantFold2(doubleValue, _ / _)
            case tpnme.Mod        => constantFold2(doubleValue, _ % _)
            case tpnme.Lt         => constantFold2(doubleValue, _ < _)
            case tpnme.Gt         => constantFold2(doubleValue, _ > _)
            case tpnme.Ge         => constantFold2(doubleValue, _ >= _)
            case tpnme.Le         => constantFold2(doubleValue, _ <= _)
            case tpnme.Min        => constantFold2(doubleValue, _ min _)
            case tpnme.Max        => constantFold2(doubleValue, _ max _)
            case tpnme.ToInt      => constantFold1(doubleValue, _.toInt)
            case tpnme.ToLong     => constantFold1(doubleValue, _.toLong)
            case tpnme.ToFloat    => constantFold1(doubleValue, _.toFloat)
            case _ => None
          else if owner == defn.CompiletimeOpsStringModuleClass then name match
            case tpnme.Plus       => constantFold2(stringValue, _ + _)
            case tpnme.Length     => constantFold1(stringValue, _.length)
            case tpnme.Matches    => constantFold2(stringValue, _ matches _)
            case tpnme.Substring  =>
              constantFold3(stringValue, intValue, intValue, (s, b, e) => s.substring(b, e))
            case tpnme.CharAt     =>
              constantFold2AB(stringValue, intValue, _ charAt _)
            case _ => None
          else if owner == defn.CompiletimeOpsBooleanModuleClass then name match
            case tpnme.Not        => constantFold1(boolValue, x => !x)
            case tpnme.And        => constantFold2(boolValue, _ && _)
            case tpnme.Or         => constantFold2(boolValue, _ || _)
            case tpnme.Xor        => constantFold2(boolValue, _ ^ _)
            case _ => None
          else None

        constantType.getOrElse(NoType)
      }

    case _ => NoType
  end tryCompiletimeConstantFold
end TypeEval
