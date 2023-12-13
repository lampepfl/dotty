package scala.runtime

import scala.annotation.experimental

object Tuples {

  inline val MaxSpecialized = 22

  def toArray(self: Tuple): Array[Object] = (self: Any) match {
    case EmptyTuple => Array.emptyObjectArray
    case self: TupleXXL => self.toArray
    case self: Product => productToArray(self)
  }

  def toIArray(self: Tuple): IArray[Object] = (self: Any) match {
    case EmptyTuple => Array.emptyObjectArray.asInstanceOf[IArray[Object]]
    case self: TupleXXL => self.elems
    case self: Product => productToArray(self).asInstanceOf[IArray[Object]]
  }

  def productToArray(self: Product): Array[Object] = {
    val arr = new Array[Object](self.productArity)
    var i = 0
    while (i < arr.length) {
      arr(i) = self.productElement(i).asInstanceOf[Object]
      i += 1
    }
    arr
  }

  def fromArray(xs: Array[Object], n: Int): Tuple = n match {
    case 0  => EmptyTuple
    case 1  => Tuple1(xs(0))
    case 2  => Tuple2(xs(0), xs(1))
    case 3  => Tuple3(xs(0), xs(1), xs(2))
    case 4  => Tuple4(xs(0), xs(1), xs(2), xs(3))
    case 5  => Tuple5(xs(0), xs(1), xs(2), xs(3), xs(4))
    case 6  => Tuple6(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5))
    case 7  => Tuple7(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6))
    case 8  => Tuple8(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7))
    case 9  => Tuple9(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8))
    case 10 => Tuple10(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9))
    case 11 => Tuple11(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10))
    case 12 => Tuple12(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11))
    case 13 => Tuple13(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12))
    case 14 => Tuple14(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13))
    case 15 => Tuple15(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14))
    case 16 => Tuple16(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15))
    case 17 => Tuple17(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16))
    case 18 => Tuple18(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17))
    case 19 => Tuple19(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18))
    case 20 => Tuple20(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19))
    case 21 => Tuple21(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20))
    case 22 => Tuple22(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), xs(9), xs(10), xs(11), xs(12), xs(13), xs(14), xs(15), xs(16), xs(17), xs(18), xs(19), xs(20), xs(21))
    case _ => TupleXXL.fromIArray(xs.clone().asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]
  }

  def fromArray(xs: Array[Object]): Tuple = fromArray(xs, xs.length)

  def fromIArray(xs: IArray[Object], n: Int): Tuple =
    if n <= 22 || n != xs.length
    then fromArray(xs.asInstanceOf[Array[Object]], n)
    else TupleXXL.fromIArray(xs).asInstanceOf[Tuple]

  def fromIArray(xs: IArray[Object]): Tuple = fromIArray(xs, xs.length)

  def fromProduct(xs: Product): Tuple = (xs.productArity match {
    case 0  => EmptyTuple
    case 1 =>
      xs match {
        case xs: Tuple1[?] => xs
        case xs => Tuple1(xs.productElement(0))
      }
    case 2 =>
      xs match {
        case xs: Tuple2[?, ?] => xs
        case xs => Tuple2(xs.productElement(0), xs.productElement(1))
      }
    case 3 =>
      xs match {
        case xs: Tuple3[?, ?, ?] => xs
        case xs => Tuple3(xs.productElement(0), xs.productElement(1), xs.productElement(2))
      }
    case 4 =>
      xs match {
        case xs: Tuple4[?, ?, ?, ?] => xs
        case xs => Tuple4(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3))
      }
    case 5 =>
      xs match {
        case xs: Tuple5[?, ?, ?, ?, ?] => xs
        case xs => Tuple5(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4))
      }
    case 6 =>
      xs match {
        case xs: Tuple6[?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple6(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5))
      }
    case 7 =>
      xs match {
        case xs: Tuple7[?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple7(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6))
      }
    case 8 =>
      xs match {
        case xs: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple8(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7))
      }
    case 9 =>
      xs match {
        case xs: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple9(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8))
      }
    case 10 =>
      xs match {
        case xs: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple10(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9))
      }
    case 11 =>
      xs match {
        case xs: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple11(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10))
      }
    case 12 =>
      xs match {
        case xs: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple12(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11))
      }
    case 13 =>
      xs match {
        case xs: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple13(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12))
      }
    case 14 =>
      xs match {
        case xs: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple14(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13))
      }
    case 15 =>
      xs match {
        case xs: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple15(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14))
      }
    case 16 =>
      xs match {
        case xs: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple16(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15))
      }
    case 17 =>
      xs match {
        case xs: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple17(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16))
      }
    case 18 =>
      xs match {
        case xs: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple18(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17))
      }
    case 19 =>
      xs match {
        case xs: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple19(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18))
      }
    case 20 =>
      xs match {
        case xs: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple20(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19))
      }
    case 21 =>
      xs match {
        case xs: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple21(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19), xs.productElement(20))
      }
    case 22 =>
      xs match {
        case xs: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] => xs
        case xs => Tuple22(xs.productElement(0), xs.productElement(1), xs.productElement(2), xs.productElement(3), xs.productElement(4), xs.productElement(5), xs.productElement(6), xs.productElement(7), xs.productElement(8), xs.productElement(9), xs.productElement(10), xs.productElement(11), xs.productElement(12), xs.productElement(13), xs.productElement(14), xs.productElement(15), xs.productElement(16), xs.productElement(17), xs.productElement(18), xs.productElement(19), xs.productElement(20), xs.productElement(21))
      }
    case _ =>
      (xs match {
        case xs: TupleXXL => xs
        case xs => TupleXXL.fromIArray(xs.productIterator.map(_.asInstanceOf[Object]).toArray.asInstanceOf[IArray[Object]]) // TODO use Iterator.toIArray
      }).asInstanceOf[Tuple]
  })

  // Cons for Tuple1 to Tuple22
  private def specialCaseCons(x: Any, self: Tuple): Tuple = {
    (self: Any) match {
      case EmptyTuple =>
        Tuple1(x)
      case self: Tuple1[?] =>
        Tuple2(x, self._1)
      case self: Tuple2[?, ?] =>
        Tuple3(x, self._1, self._2)
      case self: Tuple3[?, ?, ?] =>
        Tuple4(x, self._1, self._2, self._3)
      case self: Tuple4[?, ?, ?, ?] =>
        Tuple5(x, self._1, self._2, self._3, self._4)
      case self: Tuple5[?, ?, ?, ?, ?] =>
        Tuple6(x, self._1, self._2, self._3, self._4, self._5)
      case self: Tuple6[?, ?, ?, ?, ?, ?] =>
        Tuple7(x, self._1, self._2, self._3, self._4, self._5, self._6)
      case self: Tuple7[?, ?, ?, ?, ?, ?, ?] =>
        Tuple8(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7)
      case self: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple9(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8)
      case self: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple10(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9)
      case self: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple11(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10)
      case self: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple12(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11)
      case self: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple13(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12)
      case self: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple14(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13)
      case self: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple15(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14)
      case self: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple16(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15)
      case self: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple17(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16)
      case self: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple18(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17)
      case self: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple19(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18)
      case self: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple20(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19)
      case self: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple21(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20)
      case self: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple22(x, self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, self._21)
      case self: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        val arr: Array[Object] = Array(
          x.asInstanceOf[Object], self._1.asInstanceOf[Object], self._2.asInstanceOf[Object],
          self._3.asInstanceOf[Object], self._4.asInstanceOf[Object], self._5.asInstanceOf[Object],
          self._6.asInstanceOf[Object], self._7.asInstanceOf[Object], self._8.asInstanceOf[Object],
          self._9.asInstanceOf[Object], self._10.asInstanceOf[Object], self._11.asInstanceOf[Object],
          self._12.asInstanceOf[Object], self._13.asInstanceOf[Object], self._14.asInstanceOf[Object],
          self._15.asInstanceOf[Object], self._16.asInstanceOf[Object], self._17.asInstanceOf[Object],
          self._18.asInstanceOf[Object], self._19.asInstanceOf[Object], self._20.asInstanceOf[Object],
          self._21.asInstanceOf[Object], self._22.asInstanceOf[Object],
        )
        TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]
    }
  }

  // Cons for TupleXXL
  private def xxlCons(x: Any, xxl: TupleXXL): TupleXXL = {
    val arr = new Array[Object](xxl.productArity + 1)
    arr(0) = x.asInstanceOf[Object]
    System.arraycopy(xxl.elems, 0, arr, 1, xxl.productArity)
    TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]])
  }

  def cons(x: Any, self: Tuple): Tuple = (self: Any) match {
    case xxl: TupleXXL => xxlCons(x, xxl).asInstanceOf[Tuple]
    case _ => specialCaseCons(x, self)
  }

  def concat[This <: Tuple, That <: Tuple](self: This, that: That): Tuple = {
    val selfSize: Int = self.size
    // If one of the tuples is empty, we can leave early
    if selfSize == 0 then
      return that

    val thatSize: Int = that.size
    if thatSize == 0 then
      return self

    val arr = new Array[Object](selfSize + thatSize)

    // Copies the tuple to an array, at the given offset
    inline def copyToArray[T <: Tuple](tuple: T, size: Int, array: Array[Object], offset: Int): Unit = (tuple: Any) match {
      case xxl: TupleXXL =>
        System.arraycopy(xxl.elems, 0, array, offset, size)
      case _ =>
        tuple.productIterator.asInstanceOf[Iterator[Object]]
          .copyToArray(array, offset, size)
    }

    // In the general case, we copy the two tuples to an array, and convert it back to a tuple
    copyToArray(self, selfSize, arr, 0)
    copyToArray(that, thatSize, arr, selfSize)
    fromIArray(arr.asInstanceOf[IArray[Object]])
  }

  def size(self: Tuple): Int = (self: Any) match {
    case EmptyTuple => 0
    case self: Product => self.productArity
  }

  // Tail for Tuple1 to Tuple22
  private def specialCaseTail(self: Tuple): Tuple = {
    (self: Any) match {
      case self: Tuple1[?] =>
        EmptyTuple
      case self: Tuple2[?, ?] =>
        Tuple1(self._2)
      case self: Tuple3[?, ?, ?] =>
        Tuple2(self._2, self._3)
      case self: Tuple4[?, ?, ?, ?] =>
        Tuple3(self._2, self._3, self._4)
      case self: Tuple5[?, ?, ?, ?, ?] =>
        Tuple4(self._2, self._3, self._4, self._5)
      case self: Tuple6[?, ?, ?, ?, ?, ?] =>
        Tuple5(self._2, self._3, self._4, self._5, self._6)
      case self: Tuple7[?, ?, ?, ?, ?, ?, ?] =>
        Tuple6(self._2, self._3, self._4, self._5, self._6, self._7)
      case self: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple7(self._2, self._3, self._4, self._5, self._6, self._7, self._8)
      case self: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple8(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9)
      case self: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple9(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10)
      case self: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple10(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11)
      case self: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple11(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12)
      case self: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple12(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13)
      case self: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple13(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14)
      case self: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple14(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15)
      case self: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple15(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16)
      case self: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple16(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17)
      case self: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple17(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18)
      case self: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple18(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19)
      case self: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple19(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20)
      case self: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple20(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, self._21)
      case self: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple21(self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, self._21, self._22)
    }
  }

  // Tail for TupleXXL
  private def xxlTail(xxl: TupleXXL): Tuple = {
    if (xxl.productArity == 23) {
      val elems = xxl.elems
      Tuple22(
        elems(1), elems(2), elems(3), elems(4), elems(5), elems(6), elems(7),
        elems(8), elems(9), elems(10), elems(11), elems(12), elems(13), elems(14),
        elems(15), elems(16), elems(17), elems(18), elems(19), elems(20),
        elems(21), elems(22)
      )
    } else {
      val arr = new Array[Object](xxl.elems.length - 1)
      System.arraycopy(xxl.elems, 1, arr, 0, xxl.elems.length - 1)
      TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]
    }
  }

  def tail(self: NonEmptyTuple): Tuple = (self: Any) match {
    case xxl: TupleXXL => xxlTail(xxl)
    case _ => specialCaseTail(self)
  }

  // Append for TupleXXL
  private def xxlAppend(x: Any, xxl: TupleXXL): TupleXXL = {
    val arr = new Array[Object](xxl.productArity + 1)
    arr(xxl.productArity) = x.asInstanceOf[Object]
    System.arraycopy(xxl.elems, 0, arr, 0, xxl.productArity)
    TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]])
  }

  // Append for Tuple1 to Tuple22
  private def specialCaseAppend(x: Any, self: Tuple): Tuple = {
    (self: Any) match {
      case EmptyTuple =>
        Tuple1(x)
      case self: Tuple1[?] =>
        Tuple2(self._1, x)
      case self: Tuple2[?, ?] =>
        Tuple3(self._1, self._2, x)
      case self: Tuple3[?, ?, ?] =>
        Tuple4(self._1, self._2, self._3, x)
      case self: Tuple4[?, ?, ?, ?] =>
        Tuple5(self._1, self._2, self._3, self._4, x)
      case self: Tuple5[?, ?, ?, ?, ?] =>
        Tuple6(self._1, self._2, self._3, self._4, self._5, x)
      case self: Tuple6[?, ?, ?, ?, ?, ?] =>
        Tuple7(self._1, self._2, self._3, self._4, self._5, self._6, x)
      case self: Tuple7[?, ?, ?, ?, ?, ?, ?] =>
        Tuple8(self._1, self._2, self._3, self._4, self._5, self._6, self._7, x)
      case self: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple9(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, x)
      case self: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple10(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, x)
      case self: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple11(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, x)
      case self: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple12(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, x)
      case self: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple13(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, x)
      case self: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple14(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, x)
      case self: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple15(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, x)
      case self: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple16(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, x)
      case self: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple17(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, x)
      case self: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple18(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, x)
      case self: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple19(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, x)
      case self: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple20(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, x)
      case self: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple21(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, x)
      case self: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple22(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, self._21, x)
      case self: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        val arr: Array[Object] = Array(
          self._1.asInstanceOf[Object], self._2.asInstanceOf[Object],
          self._3.asInstanceOf[Object], self._4.asInstanceOf[Object], self._5.asInstanceOf[Object],
          self._6.asInstanceOf[Object], self._7.asInstanceOf[Object], self._8.asInstanceOf[Object],
          self._9.asInstanceOf[Object], self._10.asInstanceOf[Object], self._11.asInstanceOf[Object],
          self._12.asInstanceOf[Object], self._13.asInstanceOf[Object], self._14.asInstanceOf[Object],
          self._15.asInstanceOf[Object], self._16.asInstanceOf[Object], self._17.asInstanceOf[Object],
          self._18.asInstanceOf[Object], self._19.asInstanceOf[Object], self._20.asInstanceOf[Object],
          self._21.asInstanceOf[Object], self._22.asInstanceOf[Object], x.asInstanceOf[Object]
        )
        TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]
    }
  }

  def append(x: Any, self: Tuple): Tuple = (self: Any) match {
    case xxl: TupleXXL => xxlAppend(x, xxl).asInstanceOf[Tuple]
    case _ => specialCaseAppend(x, self)
  }

  // Init for TupleXXL
  private def xxlInit(xxl: TupleXXL): Tuple = {
    if (xxl.productArity == 23) {
      val elems = xxl.elems
      Tuple22(
        elems(0), elems(1), elems(2), elems(3), elems(4), elems(5),
        elems(6), elems(7), elems(8), elems(9), elems(10), elems(11),
        elems(12), elems(13), elems(14), elems(15), elems(16), elems(17),
        elems(18), elems(19), elems(20), elems(21)
      )
    } else {
      val arr = new Array[Object](xxl.elems.length - 1)
      System.arraycopy(xxl.elems, 0, arr, 0, xxl.elems.length - 1)
      TupleXXL.fromIArray(arr.asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]
    }
  }

  // Reverse for TupleXXL
  private def xxlReverse(xxl: TupleXXL): Tuple =
    TupleXXL.fromIArray(xxl.elems.reverse.asInstanceOf[IArray[Object]]).asInstanceOf[Tuple]

  // Reverse for Tuple0 to Tuple22
  private def specialCaseReverse(self: Tuple): Tuple = {
    (self: Any) match {
      case EmptyTuple =>
        EmptyTuple
      case self: Tuple1[?] =>
        self
      case self: Tuple2[?, ?] =>
        Tuple2(self._2, self._1)
      case self: Tuple3[?, ?, ?] =>
        Tuple3(self._3, self._2, self._1)
      case self: Tuple4[?, ?, ?, ?] =>
        Tuple4(self._4, self._3, self._2, self._1)
      case self: Tuple5[?, ?, ?, ?, ?] =>
        Tuple5(self._5, self._4, self._3, self._2, self._1)
      case self: Tuple6[?, ?, ?, ?, ?, ?] =>
        Tuple6(self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple7[?, ?, ?, ?, ?, ?, ?] =>
        Tuple7(self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple8(self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple9(self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple10(self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple11(self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple12(self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple13(self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple14(self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple15(self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple16(self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple17(self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple18(self._18, self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple19(self._19, self._18, self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple20(self._20, self._19, self._18, self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple21(self._21, self._20, self._19, self._18, self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
      case self: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple22(self._22, self._21, self._20, self._19, self._18, self._17, self._16, self._15, self._14, self._13, self._12, self._11, self._10, self._9, self._8, self._7, self._6, self._5, self._4, self._3, self._2, self._1)
    }
  }

  def reverse(self: Tuple): Tuple = (self: Any) match {
    case xxl: TupleXXL => xxlReverse(xxl)
    case _ => specialCaseReverse(self)
  }

  // Init for Tuple1 to Tuple22
  private def specialCaseInit(self: Tuple): Tuple = {
    (self: Any) match {
      case _: Tuple1[?] =>
        EmptyTuple
      case self: Tuple2[?, ?] =>
        Tuple1(self._1)
      case self: Tuple3[?, ?, ?] =>
        Tuple2(self._1, self._2)
      case self: Tuple4[?, ?, ?, ?] =>
        Tuple3(self._1, self._2, self._3)
      case self: Tuple5[?, ?, ?, ?, ?] =>
        Tuple4(self._1,self._2, self._3, self._4)
      case self: Tuple6[?, ?, ?, ?, ?, ?] =>
        Tuple5(self._1, self._2, self._3, self._4, self._5)
      case self: Tuple7[?, ?, ?, ?, ?, ?, ?] =>
        Tuple6(self._1, self._2, self._3, self._4, self._5, self._6)
      case self: Tuple8[?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple7(self._1, self._2, self._3, self._4, self._5, self._6, self._7)
      case self: Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple8(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8)
      case self: Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple9(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9)
      case self: Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple10(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10)
      case self: Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple11(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11)
      case self: Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple12(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12)
      case self: Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple13(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13)
      case self: Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple14(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14)
      case self: Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple15(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15)
      case self: Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple16(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16)
      case self: Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple17(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17)
      case self: Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple18(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18)
      case self: Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple19(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19)
      case self: Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple20(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20)
      case self: Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?] =>
        Tuple21(self._1, self._2, self._3, self._4, self._5, self._6, self._7, self._8, self._9, self._10, self._11, self._12, self._13, self._14, self._15, self._16, self._17, self._18, self._19, self._20, self._21)
    }
  }

  def init(self: NonEmptyTuple): Tuple = (self: Any) match {
    case xxl: TupleXXL => xxlInit(xxl)
    case _ => specialCaseInit(self)
  }

  def last(self: NonEmptyTuple): Any = (self: Any) match {
    case self: Product => self.productElement(self.productArity - 1)
  }

  def apply(self: NonEmptyTuple, n: Int): Any =
    self.productElement(n)

  // Benchmarks showed that this is faster than doing (it1 zip it2).copyToArray(...)
  private def zipIterators(it1: Iterator[Any], it2: Iterator[Any], size: Int): IArray[Object] = {
    val arr = new Array[Object](size)
    var i = 0
    while (i < size) {
      arr(i) = (it1.next(), it2.next())
      i += 1
    }
    arr.asInstanceOf[IArray[Object]]
  }

  def zip(t1: Tuple, t2: Tuple): Tuple = {
    val t1Size: Int = t1.size
    val t2Size: Int = t2.size
    val size = Math.min(t1Size, t2Size)
    if size == 0 then EmptyTuple
    else Tuple.fromIArray(
      zipIterators(
        t1.productIterator,
        t2.productIterator,
        size
      )
    )
  }

  def map[F[_]](self: Tuple, f: [t] => t => F[t]): Tuple = self match {
    case EmptyTuple => self
    case _ => fromIArray(self.productIterator.map(f(_).asInstanceOf[Object]).toArray.asInstanceOf[IArray[Object]]) // TODO use toIArray
  }

  def take(self: Tuple, n: Int): Tuple = {
    if (n < 0) throw new IndexOutOfBoundsException(n.toString)
    val selfSize: Int = self.size
    val actualN = Math.min(n, selfSize)

    if (actualN == 0) EmptyTuple
    else {
      val arr = (self: Any) match {
        case xxl: TupleXXL =>
          xxl.elems.asInstanceOf[Array[Object]].take(actualN)
        case _ =>
          val arr = new Array[Object](actualN)
          self.productIterator.asInstanceOf[Iterator[Object]]
            .copyToArray(arr, 0, actualN)
          arr
      }

      fromIArray(arr.asInstanceOf[IArray[Object]])
    }
  }

  def drop(self: Tuple, n: Int): Tuple = {
    if (n < 0) throw new IndexOutOfBoundsException(n.toString)
    val size = self.size
    val actualN = Math.min(n, size)
    val rem = size - actualN

    if (rem == 0) EmptyTuple
    else {
      val arr = (self: Any) match {
        case xxl: TupleXXL =>
          xxl.elems.asInstanceOf[Array[Object]].drop(actualN)
        case _ =>
          val arr = new Array[Object](rem)
          self.productIterator.asInstanceOf[Iterator[Object]]
            .drop(actualN).copyToArray(arr, 0, rem)
          arr
      }

      fromIArray(arr.asInstanceOf[IArray[Object]])
    }
  }

  def splitAt(self: Tuple, n: Int): (Tuple, Tuple) = {
    if (n < 0) throw new IndexOutOfBoundsException(n.toString)
    val size = self.size
    val actualN = Math.min(n, size)
    val (arr1, arr2) = (self: Any) match {
      case EmptyTuple => (Array.empty[Object], Array.empty[Object])
      case xxl: TupleXXL =>
        xxl.elems.asInstanceOf[Array[Object]].splitAt(actualN)
      case _ =>
        val arr1 = new Array[Object](actualN)
        val arr2 = new Array[Object](size - actualN)
        val it = self.productIterator.asInstanceOf[Iterator[Object]]
        it.copyToArray(arr1, 0, actualN)
        it.copyToArray(arr2, 0, size - actualN)
        (arr1, arr2)
    }

    (
      fromIArray(arr1.asInstanceOf[IArray[Object]]),
      fromIArray(arr2.asInstanceOf[IArray[Object]])
    )
  }

  def consIterator(head: Any, tail: Tuple): Iterator[Any] =
    Iterator.single(head) ++ tail.productIterator

  def concatIterator(tup1: Tuple, tup2: Tuple): Iterator[Any] =
    tup1.productIterator ++ tup2.productIterator

  def isInstanceOfTuple(x: Any): Boolean =
    x match
      case x: Product =>
        x.productArity match
          case 0 => x == EmptyTuple
          case 1 => x.isInstanceOf[Tuple1[?]]
          case 2 => x.isInstanceOf[Tuple2[?, ?]]
          case 3 => x.isInstanceOf[Tuple3[?, ?, ?]]
          case 4 => x.isInstanceOf[Tuple4[?, ?, ?, ?]]
          case 5 => x.isInstanceOf[Tuple5[?, ?, ?, ?, ?]]
          case 6 => x.isInstanceOf[Tuple6[?, ?, ?, ?, ?, ?]]
          case 7 => x.isInstanceOf[Tuple7[?, ?, ?, ?, ?, ?, ?]]
          case 8 => x.isInstanceOf[Tuple8[?, ?, ?, ?, ?, ?, ?, ?]]
          case 9 => x.isInstanceOf[Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 10 => x.isInstanceOf[Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 11 => x.isInstanceOf[Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 12 => x.isInstanceOf[Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 13 => x.isInstanceOf[Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 14 => x.isInstanceOf[Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 15 => x.isInstanceOf[Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 16 => x.isInstanceOf[Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 17 => x.isInstanceOf[Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 18 => x.isInstanceOf[Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 19 => x.isInstanceOf[Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 20 => x.isInstanceOf[Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 21 => x.isInstanceOf[Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 22 => x.isInstanceOf[Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case _ => x.isInstanceOf[TupleXXL]
      case _ =>
        false

  def isInstanceOfEmptyTuple(x: Any): Boolean = x == EmptyTuple

  def isInstanceOfNonEmptyTuple(x: Any): Boolean =
    x match
      case x: Product =>
        x.productArity match
          case 1 => x.isInstanceOf[Tuple1[?]]
          case 2 => x.isInstanceOf[Tuple2[?, ?]]
          case 3 => x.isInstanceOf[Tuple3[?, ?, ?]]
          case 4 => x.isInstanceOf[Tuple4[?, ?, ?, ?]]
          case 5 => x.isInstanceOf[Tuple5[?, ?, ?, ?, ?]]
          case 6 => x.isInstanceOf[Tuple6[?, ?, ?, ?, ?, ?]]
          case 7 => x.isInstanceOf[Tuple7[?, ?, ?, ?, ?, ?, ?]]
          case 8 => x.isInstanceOf[Tuple8[?, ?, ?, ?, ?, ?, ?, ?]]
          case 9 => x.isInstanceOf[Tuple9[?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 10 => x.isInstanceOf[Tuple10[?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 11 => x.isInstanceOf[Tuple11[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 12 => x.isInstanceOf[Tuple12[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 13 => x.isInstanceOf[Tuple13[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 14 => x.isInstanceOf[Tuple14[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 15 => x.isInstanceOf[Tuple15[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 16 => x.isInstanceOf[Tuple16[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 17 => x.isInstanceOf[Tuple17[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 18 => x.isInstanceOf[Tuple18[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 19 => x.isInstanceOf[Tuple19[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 20 => x.isInstanceOf[Tuple20[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 21 => x.isInstanceOf[Tuple21[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case 22 => x.isInstanceOf[Tuple22[?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?]]
          case _ => x.isInstanceOf[TupleXXL]
      case _ =>
        false

}
