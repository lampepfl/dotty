package scala.quoted

import scala.reflect.ClassTag

/** A typeclass for types that can be turned to `quoted.Expr[T]`
 *  without going through an explicit `'{...}` operation.
 */
trait Liftable[T] {

  /** Lift a value into an expression containing the construction of that value */
  def toExpr(x: T): given QuoteContext => Expr[T]

}

/** Some liftable base types. To be completed with at least all types
 *  that are valid Scala literals. The actual implementation of these
 *  typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
 *  gives an alternative implementation using just the basic staging system.
 */
object Liftable {

  given Liftable_Boolean_delegate[T <: Boolean] as Liftable[T] = new PrimitiveLiftable
  given Liftable_Byte_delegate[T <: Byte] as Liftable[T] = new PrimitiveLiftable
  given Liftable_Short_delegate[T <: Short] as Liftable[T] = new PrimitiveLiftable
  given Liftable_Int_delegate[T <: Int] as Liftable[T] = new PrimitiveLiftable
  given Liftable_Long_delegate[T <: Long] as Liftable[T] = new PrimitiveLiftable
  given Liftable_Float_delegate[T <: Float] as Liftable[T] = new PrimitiveLiftable
  given Liftable_Double_delegate[T <: Double] as Liftable[T] = new PrimitiveLiftable
  given Liftable_Char_delegate[T <: Char] as Liftable[T] = new PrimitiveLiftable
  given Liftable_String_delegate[T <: String] as Liftable[T] = new PrimitiveLiftable

  private class PrimitiveLiftable[T <: Unit | Null | Int | Boolean | Byte | Short | Int | Long | Float | Double | Char | String] extends Liftable[T] {
    /** Lift a primitive value `n` into `'{ n }` */
    def toExpr(x: T) = given qctx => {
      import qctx.tasty._
      Literal(Constant(x)).seal.asInstanceOf[Expr[T]]
    }
  }

  given ClassIsLiftable[T] as Liftable[Class[T]] = new Liftable[Class[T]] {
    /** Lift a `Class[T]` into `'{ classOf[T] }` */
    def toExpr(x: Class[T]) = given qctx => {
      import qctx.tasty._
      Ref(defn.Predef_classOf).appliedToType(Type(x)).seal.asInstanceOf[Expr[Class[T]]]
    }
  }

  given ClassTagIsLiftable[T: Type] as Liftable[ClassTag[T]] = new Liftable[ClassTag[T]] {
    def toExpr(ct: ClassTag[T]): given QuoteContext => Expr[ClassTag[T]] =
      '{ ClassTag[T](${ct.runtimeClass.toExpr}) }
  }

  given ArrayIsLiftable[T: Type: Liftable: ClassTag] as Liftable[Array[T]] = new Liftable[Array[T]] {
    def toExpr(arr: Array[T]): given QuoteContext => Expr[Array[T]] =
      '{ Array[T](${arr.toSeq.toExpr}: _*)(${the[ClassTag[T]].toExpr}) }
  }

  given ArrayOfBooleanIsLiftable as Liftable[Array[Boolean]] = new Liftable[Array[Boolean]] {
    def toExpr(array: Array[Boolean]): given QuoteContext => Expr[Array[Boolean]] =
      if (array.length == 0) '{ Array.emptyBooleanArray }
      else '{ Array(${array(0).toExpr}, ${array.toSeq.tail.toExpr}: _*) }
  }

  given ArrayOfByteIsLiftable as Liftable[Array[Byte]] = new Liftable[Array[Byte]] {
    def toExpr(array: Array[Byte]): given QuoteContext => Expr[Array[Byte]] =
      if (array.length == 0) '{ Array.emptyByteArray }
      else '{ Array(${array(0).toExpr}, ${array.toSeq.tail.toExpr}: _*) }
  }

  given ArrayOfShortIsLiftable as Liftable[Array[Short]] = new Liftable[Array[Short]] {
    def toExpr(array: Array[Short]): given QuoteContext => Expr[Array[Short]] =
      if (array.length == 0) '{ Array.emptyShortArray }
      else '{ Array(${array(0).toExpr}, ${array.toSeq.tail.toExpr}: _*) }
  }

  given ArrayOfCharIsLiftable as Liftable[Array[Char]] = new Liftable[Array[Char]] {
    def toExpr(array: Array[Char]): given QuoteContext => Expr[Array[Char]] =
      if (array.length == 0) '{ Array.emptyCharArray }
      else '{ Array(${array(0).toExpr}, ${array.toSeq.tail.toExpr}: _*) }
  }

  given ArrayOfIntIsLiftable as Liftable[Array[Int]] = new Liftable[Array[Int]] {
    def toExpr(array: Array[Int]): given QuoteContext => Expr[Array[Int]] =
      if (array.length == 0) '{ Array.emptyIntArray }
      else '{ Array(${array(0).toExpr}, ${array.toSeq.tail.toExpr}: _*) }
  }

  given ArrayOfLongIsLiftable as Liftable[Array[Long]] = new Liftable[Array[Long]] {
    def toExpr(array: Array[Long]): given QuoteContext => Expr[Array[Long]] =
      if (array.length == 0) '{ Array.emptyLongArray }
      else '{ Array(${array(0).toExpr}, ${array.toSeq.tail.toExpr}: _*) }
  }

  given ArrayOfFloatIsLiftable as Liftable[Array[Float]] = new Liftable[Array[Float]] {
    def toExpr(array: Array[Float]): given QuoteContext => Expr[Array[Float]] =
      if (array.length == 0) '{ Array.emptyFloatArray }
      else '{ Array(${array(0).toExpr}, ${array.toSeq.tail.toExpr}: _*) }
  }

  given ArrayOfDoubleIsLiftable as Liftable[Array[Double]] = new Liftable[Array[Double]] {
    def toExpr(array: Array[Double]): given QuoteContext => Expr[Array[Double]] =
      if (array.length == 0) '{ Array.emptyDoubleArray }
      else '{ Array(${array(0).toExpr}, ${array.toSeq.tail.toExpr}: _*) }
  }

  given IArrayIsLiftable[T: Type] as Liftable[IArray[T]] given (ltArray: Liftable[Array[T]]) = new Liftable[IArray[T]] {
    def toExpr(iarray: IArray[T]): given QuoteContext => Expr[IArray[T]] =
      '{ ${ltArray.toExpr(iarray.asInstanceOf[Array[T]])}.asInstanceOf[IArray[T]] }
  }

  given [T: Type: Liftable] as Liftable[Seq[T]] = new Liftable[Seq[T]] {
    def toExpr(xs: Seq[T]): given QuoteContext => Expr[Seq[T]] =
      xs.map(the[Liftable[T]].toExpr).toExprOfSeq
  }

  given [T: Type: Liftable] as Liftable[List[T]] = new Liftable[List[T]] {
    def toExpr(xs: List[T]): given QuoteContext => Expr[List[T]] =
      xs.map(the[Liftable[T]].toExpr).toExprOfList
  }

  given [T: Type: Liftable] as Liftable[Set[T]] = new Liftable[Set[T]] {
    def toExpr(set: Set[T]): given QuoteContext => Expr[Set[T]] =
      '{ Set(${set.toSeq.toExpr}: _*) }
  }

  given [T: Type: Liftable, U: Type: Liftable] as Liftable[Map[T, U]] = new Liftable[Map[T, U]] {
    def toExpr(map: Map[T, U]): given QuoteContext => Expr[Map[T, U]] =
    '{ Map(${map.toSeq.toExpr}: _*) }
  }

  given [T: Type: Liftable] as Liftable[Option[T]] = new Liftable[Option[T]] {
    def toExpr(x: Option[T]): given QuoteContext => Expr[Option[T]] = x match {
      case Some(x) => '{ Some[T](${x.toExpr}) }
      case None => '{ None: Option[T] }
    }
  }

  given [L: Type: Liftable, R: Type: Liftable] as Liftable[Either[L, R]] = new Liftable[Either[L, R]] {
    def toExpr(x: Either[L, R]): given QuoteContext => Expr[Either[L, R]] = x match {
      case Left(x) => '{ Left[L, R](${x.toExpr}) }
      case Right(x) => '{ Right[L, R](${x.toExpr}) }
    }
  }

  given [T1: Type: Liftable] as Liftable[Tuple1[T1]] = new {
    def toExpr(tup: Tuple1[T1]) =
      '{ Tuple1(${tup._1.toExpr}) }
  }

  given [T1: Type: Liftable, T2: Type: Liftable] as Liftable[Tuple2[T1, T2]] = new {
    def toExpr(tup: Tuple2[T1, T2]) =
      '{ (${tup._1.toExpr}, ${tup._2.toExpr}) }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable] as Liftable[Tuple3[T1, T2, T3]] = new {
    def toExpr(tup: Tuple3[T1, T2, T3]) =
      '{ (${tup._1.toExpr}, ${tup._2.toExpr}, ${tup._3.toExpr}) }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable] as Liftable[Tuple4[T1, T2, T3, T4]] = new {
    def toExpr(tup: Tuple4[T1, T2, T3, T4]) =
      '{ (${tup._1.toExpr}, ${tup._2.toExpr}, ${tup._3.toExpr}, ${tup._4.toExpr}) }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable] as Liftable[Tuple5[T1, T2, T3, T4, T5]] = new {
    def toExpr(tup: Tuple5[T1, T2, T3, T4, T5]) = {
      val (x1, x2, x3, x4, x5) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable] as Liftable[Tuple6[T1, T2, T3, T4, T5, T6]] = new {
    def toExpr(tup: Tuple6[T1, T2, T3, T4, T5, T6]) = {
      val (x1, x2, x3, x4, x5, x6) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable] as Liftable[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = new {
    def toExpr(tup: Tuple7[T1, T2, T3, T4, T5, T6, T7]) = {
      val (x1, x2, x3, x4, x5, x6, x7) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable] as Liftable[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = new {
    def toExpr(tup: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable] as Liftable[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new {
    def toExpr(tup: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable] as Liftable[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = new {
    def toExpr(tup: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable] as Liftable[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = new {
    def toExpr(tup: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable] as Liftable[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = new {
    def toExpr(tup: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable] as Liftable[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = new {
    def toExpr(tup: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}, ${x13.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable] as Liftable[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = new {
    def toExpr(tup: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}, ${x13.toExpr}, ${x14.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable] as Liftable[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = new {
    def toExpr(tup: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}, ${x13.toExpr}, ${x14.toExpr}, ${x15.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable] as Liftable[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = new {
    def toExpr(tup: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}, ${x13.toExpr}, ${x14.toExpr}, ${x15.toExpr}, ${x16.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable] as Liftable[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = new {
    def toExpr(tup: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}, ${x13.toExpr}, ${x14.toExpr}, ${x15.toExpr}, ${x16.toExpr}, ${x17.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable, T18: Type: Liftable] as Liftable[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = new {
    def toExpr(tup: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}, ${x13.toExpr}, ${x14.toExpr}, ${x15.toExpr}, ${x16.toExpr}, ${x17.toExpr}, ${x18.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable, T18: Type: Liftable, T19: Type: Liftable] as Liftable[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = new {
    def toExpr(tup: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}, ${x13.toExpr}, ${x14.toExpr}, ${x15.toExpr}, ${x16.toExpr}, ${x17.toExpr}, ${x18.toExpr}, ${x19.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable, T18: Type: Liftable, T19: Type: Liftable, T20: Type: Liftable] as Liftable[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = new {
    def toExpr(tup: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}, ${x13.toExpr}, ${x14.toExpr}, ${x15.toExpr}, ${x16.toExpr}, ${x17.toExpr}, ${x18.toExpr}, ${x19.toExpr}, ${x20.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable, T18: Type: Liftable, T19: Type: Liftable, T20: Type: Liftable, T21: Type: Liftable] as Liftable[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = new {
    def toExpr(tup: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}, ${x13.toExpr}, ${x14.toExpr}, ${x15.toExpr}, ${x16.toExpr}, ${x17.toExpr}, ${x18.toExpr}, ${x19.toExpr}, ${x20.toExpr}, ${x21.toExpr}) }
    }
  }

  given [T1: Type: Liftable, T2: Type: Liftable, T3: Type: Liftable, T4: Type: Liftable, T5: Type: Liftable, T6: Type: Liftable, T7: Type: Liftable, T8: Type: Liftable, T9: Type: Liftable, T10: Type: Liftable, T11: Type: Liftable, T12: Type: Liftable, T13: Type: Liftable, T14: Type: Liftable, T15: Type: Liftable, T16: Type: Liftable, T17: Type: Liftable, T18: Type: Liftable, T19: Type: Liftable, T20: Type: Liftable, T21: Type: Liftable, T22: Type: Liftable] as Liftable[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = new {
    def toExpr(tup: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]) = {
      val (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22) = tup
      '{ (${x1.toExpr}, ${x2.toExpr}, ${x3.toExpr}, ${x4.toExpr}, ${x5.toExpr}, ${x6.toExpr}, ${x7.toExpr}, ${x8.toExpr}, ${x9.toExpr}, ${x10.toExpr}, ${x11.toExpr}, ${x12.toExpr}, ${x13.toExpr}, ${x14.toExpr}, ${x15.toExpr}, ${x16.toExpr}, ${x17.toExpr}, ${x18.toExpr}, ${x19.toExpr}, ${x20.toExpr}, ${x21.toExpr}, ${x22.toExpr}) }
    }
  }

  given [H: Type: Liftable, T <: Tuple: Type: Liftable] as Liftable[H *: T] = new {
    def toExpr(tup: H *: T): given QuoteContext => Expr[H *: T] =
      '{ ${the[Liftable[H]].toExpr(tup.head)} *: ${the[Liftable[T]].toExpr(tup.tail)} }
      // '{ ${tup.head.toExpr} *: ${tup.tail.toExpr} } // TODO figure out why this fails during CI documentation
  }

  given as Liftable[BigInt] = new Liftable[BigInt] {
    def toExpr(x: BigInt): given QuoteContext => Expr[BigInt] =
      '{ BigInt(${x.toByteArray.toExpr}) }
  }

  /** Lift a BigDecimal using the default MathContext */
  given as Liftable[BigDecimal] = new Liftable[BigDecimal] {
    def toExpr(x: BigDecimal): given QuoteContext => Expr[BigDecimal] =
      '{ BigDecimal(${x.toString.toExpr}) }
  }

}
