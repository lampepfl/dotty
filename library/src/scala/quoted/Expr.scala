package scala.quoted

/** Quoted expression of type `T`.
 *
 *  `Expr` has extension methods that are defined in `scala.quoted.Quotes`.
 */
abstract class Expr[+T] private[scala] ()

/** Constructors for expressions */
object Expr {

  /** `e.betaReduce` returns an expression that is functionally equivalent to `e`,
   *  however if `e` is of the form `((y1, ..., yn) => e2)(e1, ..., en)`
   *  then it optimizes the top most call by returning the result of beta-reducing the application.
   *  Similarly, all outermost curried function applications will be beta-reduced, if possible.
   *  Otherwise returns `expr`.
   *
   *  To retain semantics the argument `ei` is bound as `val yi = ei` and by-name arguments to `def yi = ei`.
   *  Some bindings may be elided as an early optimization.
   *
   *  Example:
   *  ```scala sc:nocompile
   *  ((a: Int, b: Int) => a + b).apply(x, y)
   *  ```
   *  will be reduced to
   *  ```scala sc:nocompile
   *  val a = x
   *  val b = y
   *  a + b
   *  ```
   *
   *  Generally:
   *  ```scala sc:nocompile
   *  ([X1, Y1, ...] => (x1, y1, ...) => ... => [Xn, Yn, ...] => (xn, yn, ...) => f[X1, Y1, ..., Xn, Yn, ...](x1, y1, ..., xn, yn, ...))).apply[Tx1, Ty1, ...](myX1, myY1, ...)....apply[Txn, Tyn, ...](myXn, myYn, ...)
   *  ```
   *  will be reduced to
   *  ```scala sc:nocompile
   *  type X1 = Tx1
   *  type Y1 = Ty1
   *  ...
   *  val x1 = myX1
   *  val y1 = myY1
   *  ...
   *  type Xn = Txn
   *  type Yn = Tyn
   *  ...
   *  val xn = myXn
   *  val yn = myYn
   *  ...
   *  f[X1, Y1, ..., Xn, Yn, ...](x1, y1, ..., xn, yn, ...)
   *  ```
   */
  def betaReduce[T](expr: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect.*
    Term.betaReduce(expr.asTerm) match
      case Some(expr1) => expr1.asExpr.asInstanceOf[Expr[T]]
      case _ => expr

  /** Returns an expression containing a block with the given statements and ending with the expression
   *  Given list of statements `s1 :: s2 :: ... :: Nil` and an expression `e` the resulting expression
   *  will be equivalent to `'{ $s1; $s2; ...; $e }`.
   */
  def block[T](statements: List[Expr[Any]], expr: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*
    Block(statements.map(asTerm), expr.asTerm).asExpr.asInstanceOf[Expr[T]]
  }

  /** Creates an expression that will construct the value `x` */
  def apply[T](x: T)(using ToExpr[T])(using Quotes): Expr[T] =
    scala.Predef.summon[ToExpr[T]].apply(x)

  /** Get `Some` of a copy of the value if the expression contains a literal constant or constructor of `T`.
   *  Otherwise returns `None`.
   *
   *  Usage:
   *  ```scala sc:nocompile
   *  case '{ ... ${expr @ Expr(value)}: T ...} =>
   *    // expr: Expr[T]
   *    // value: T
   *  ```
   *
   *  To directly get the value of an expression `expr: Expr[T]` consider using `expr.value`/`expr.valueOrError` instead.
   */
  def unapply[T](x: Expr[T])(using FromExpr[T])(using Quotes): Option[T] =
    scala.Predef.summon[FromExpr[T]].unapply(x)

  /** Creates an expression that will construct a copy of this sequence
   *
   *  Transforms a sequence of expression
   *    `Seq(e1, e2, ...)` where `ei: Expr[T]`
   *  to an expression equivalent to
   *    `'{ Seq($e1, $e2, ...) }` typed as an `Expr[Seq[T]]`
   */
  def ofSeq[T](xs: Seq[Expr[T]])(using Type[T])(using Quotes): Expr[Seq[T]] =
    Varargs(xs)

  /** Creates an expression that will construct a copy of this list
   *
   *  Transforms a list of expression
   *    `List(e1, e2, ...)` where `ei: Expr[T]`
   *  to an expression equivalent to
   *    `'{ List($e1, $e2, ...) }` typed as an `Expr[List[T]]`
   */
  def ofList[T](xs: Seq[Expr[T]])(using Type[T])(using Quotes): Expr[List[T]] =
    if xs.isEmpty then Expr(Nil) else '{ List(${Varargs(xs)}*) }

  /** Creates an expression that will construct a copy of this tuple
   *
   *  Transforms a sequence of expression
   *    `Seq(e1, e2, ...)` where `ei: Expr[Any]`
   *  to an expression equivalent to
   *    `'{ ($e1, $e2, ...) }` typed as an `Expr[Tuple]`
   */
  def ofTupleFromSeq(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] = {
    seq.size match {
      case 0 => '{ Tuple() }
      case 1 => ofTupleFromSeq1(seq)
      case 2 => ofTupleFromSeq2(seq)
      case 3 => ofTupleFromSeq3(seq)
      case 4 => ofTupleFromSeq4(seq)
      case 5 => ofTupleFromSeq5(seq)
      case 6 => ofTupleFromSeq6(seq)
      case 7 => ofTupleFromSeq7(seq)
      case 8 => ofTupleFromSeq8(seq)
      case 9 => ofTupleFromSeq9(seq)
      case 10 => ofTupleFromSeq10(seq)
      case 11 => ofTupleFromSeq11(seq)
      case 12 => ofTupleFromSeq12(seq)
      case 13 => ofTupleFromSeq13(seq)
      case 14 => ofTupleFromSeq14(seq)
      case 15 => ofTupleFromSeq15(seq)
      case 16 => ofTupleFromSeq16(seq)
      case 17 => ofTupleFromSeq17(seq)
      case 18 => ofTupleFromSeq18(seq)
      case 19 => ofTupleFromSeq19(seq)
      case 20 => ofTupleFromSeq20(seq)
      case 21 => ofTupleFromSeq21(seq)
      case 22 => ofTupleFromSeq22(seq)
      case _ => ofTupleFromSeqXXL(seq)
    }
  }

  private def ofTupleFromSeq1(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }) => '{ Tuple1($x1) }

  private def ofTupleFromSeq2(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }) => '{ Tuple2($x1, $x2) }

  private def ofTupleFromSeq3(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }) => '{ Tuple3($x1, $x2, $x3) }

  private def ofTupleFromSeq4(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }) =>
       '{ Tuple4($x1, $x2, $x3, $x4) }

  private def ofTupleFromSeq5(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }) =>
        '{ Tuple5($x1, $x2, $x3, $x4, $x5) }

  private def ofTupleFromSeq6(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }) =>
        '{ Tuple6($x1, $x2, $x3, $x4, $x5, $x6) }

  private def ofTupleFromSeq7(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }) =>
        '{ Tuple7($x1, $x2, $x3, $x4, $x5, $x6, $x7) }

  private def ofTupleFromSeq8(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }) =>
        '{ Tuple8($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8) }

  private def ofTupleFromSeq9(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }) =>
        '{ Tuple9($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9) }

  private def ofTupleFromSeq10(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }) =>
        '{ Tuple10($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10) }

  private def ofTupleFromSeq11(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }) =>
        '{ Tuple11($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11) }

  private def ofTupleFromSeq12(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }) =>
        '{ Tuple12($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12) }

  private def ofTupleFromSeq13(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }) =>
        '{ Tuple13($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13) }

  private def ofTupleFromSeq14(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }) =>
        '{ Tuple14($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14) }

  private def ofTupleFromSeq15(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }) =>
        '{ Tuple15($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15) }

  private def ofTupleFromSeq16(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }) =>
        '{ Tuple16($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16) }

  private def ofTupleFromSeq17(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }) =>
        '{ Tuple17($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17) }

  private def ofTupleFromSeq18(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }, '{ $x18: t18 }) =>
        '{ Tuple18($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18) }

  private def ofTupleFromSeq19(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }, '{ $x18: t18 }, '{ $x19: t19 }) =>
        '{ Tuple19($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19) }

  private def ofTupleFromSeq20(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }, '{ $x18: t18 }, '{ $x19: t19 }, '{ $x20: t20 }) =>
        '{ Tuple20($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20) }

  private def ofTupleFromSeq21(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }, '{ $x18: t18 }, '{ $x19: t19 }, '{ $x20: t20 }, '{ $x21: t21 }) =>
        '{ Tuple21($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20, $x21) }

  private def ofTupleFromSeq22(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }, '{ $x18: t18 }, '{ $x19: t19 }, '{ $x20: t20 }, '{ $x21: t21 }, '{ $x22: t22 }) =>
        '{ Tuple22($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20, $x21, $x22) }

  private def ofTupleFromSeqXXL(seq: Seq[Expr[Any]])(using Quotes): Expr[Tuple] =
    val tupleTpe = tupleTypeFromSeq(seq)
    tupleTpe.asType match
      case '[tpe] =>
        '{ Tuple.fromIArray(IArray(${Varargs(seq)}*)).asInstanceOf[tpe & Tuple] }

  private def tupleTypeFromSeq(seq: Seq[Expr[Any]])(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val consRef = Symbol.classSymbol("scala.*:").typeRef
    seq.foldLeft(TypeRepr.of[EmptyTuple]) { (ts, expr) =>
      AppliedType(consRef, expr.asTerm.tpe :: ts :: Nil)
    }

  /** Given a tuple of the form `(Expr[A1], ..., Expr[An])`, outputs a tuple `Expr[(A1, ..., An)]`. */
  def ofTuple[T <: Tuple: Tuple.IsMappedBy[Expr]: Type](tup: T)(using Quotes): Expr[Tuple.InverseMap[T, Expr]] = {
    val elems: Seq[Expr[Any]] = tup.asInstanceOf[Product].productIterator.toSeq.asInstanceOf[Seq[Expr[Any]]]
    ofTupleFromSeq(elems).asExprOf[Tuple.InverseMap[T, Expr]]
  }

  /** Find a given instance of type `T` in the current scope.
   *  Return `Some` containing the expression of the implicit or
   * `None` if implicit resolution failed.
   *
   *  @tparam T type of the implicit parameter
   */
  def summon[T](using Type[T])(using Quotes): Option[Expr[T]] = {
    import quotes.reflect.*
    Implicits.search(TypeRepr.of[T]) match {
      case iss: ImplicitSearchSuccess => Some(iss.tree.asExpr.asInstanceOf[Expr[T]])
      case isf: ImplicitSearchFailure => None
    }
  }

}
