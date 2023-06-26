package dotty.tools.benchmarks.inlinetraits
package specialized

/*
 * This implementation relies on the @specialized tag to specialize the MatrixLib.
 * However, @specialized does nothing in Scala 3. Therefore, an equivalent version is
 * recreated by hand further down, and the actual Scala 2 code is provided below:
 *
 *   import scala.reflect.ClassTag
 *   import scala.specialized
 *
 *   class MatrixLib[@specialized(Int) T: ClassTag] {
 *     type Matrix = Array[Array[T]]
 *
 *     object Matrix {
 *       def apply(rows: Seq[T]*): Matrix =
 *         rows.map(_.toArray).toArray
 *     }
 *
 *     def get(m: Matrix)(x: Int)(y: Int): T = m(x)(y)
 *     def rows(m: Matrix): Int = m.length
 *     def cols(m: Matrix): Int = m(0).length
 *   }
 *
 *   object IntMatrixLib extends MatrixLib[Int] {
 *     def +(m: Matrix)(n: Matrix): Matrix = {
 *       val sum = {
 *         for (row <- 0 until rows(m))
 *           yield {
 *             for (col <- 0 until cols(m))
 *               yield m(row)(col) + n(row)(col)
 *           }
 *       }
 *       Matrix(sum: _*)
 *     }
 *
 *     def *(m: Matrix)(n: Matrix): Matrix = {
 *       val prod = {
 *         for (i <- 0 until rows(m))
 *           yield {
 *             for (j <- 0 until cols(n))
 *               yield {
 *                 val mults =
 *                   for (k <- 0 until rows(n)) yield get(m)(i)(k) * get(n)(k)(j)
 *                 mults.fold(0)(_ + _)
 *               }
 *           }
 *       }
 *       Matrix(prod: _*)
 *     }
 *   }
 */

import scala.reflect.ClassTag
import scala.specialized

class MatrixLib {
  private[specialized] type ObjectMatrix = Array[Array[Object]]
  private[specialized] type IntMatrix = Array[Array[Int]]

  def get(m: ObjectMatrix)(x: Int)(y: Int): Object = m(x)(y)
  def rows(m: ObjectMatrix): Int = m.length
  def cols(m: ObjectMatrix): Int = m(0).length
  private[specialized] def get$mcI$sp(m: IntMatrix)(x: Int)(y: Int): Int = Int.unbox(get(m.asInstanceOf[ObjectMatrix])(x)(y))
  private[specialized] def rows$mcI$sp(m: IntMatrix): Int = rows(m.asInstanceOf[ObjectMatrix])
  private[specialized] def cols$mcI$sp(m: IntMatrix): Int = cols(m.asInstanceOf[ObjectMatrix])
}

private class MatrixLib$mcI$sp extends MatrixLib {
  override def get(m: ObjectMatrix)(x: Int)(y: Int): Object = Int.box(get(m.asInstanceOf[IntMatrix])(x)(y))
  override def rows(m: ObjectMatrix): Int = rows(m.asInstanceOf[IntMatrix])
  override def cols(m: ObjectMatrix): Int = cols(m.asInstanceOf[IntMatrix])
  def get(m: IntMatrix)(x: Int)(y: Int): Int = get$mcI$sp(m)(x)(y)
  def rows(m: IntMatrix): Int = rows$mcI$sp(m)
  def cols(m: IntMatrix): Int = cols$mcI$sp(m)
  override private[specialized] def get$mcI$sp(m: IntMatrix)(x: Int)(y: Int): Int = m(x)(y)
  override private[specialized] def rows$mcI$sp(m: IntMatrix): Int = m.length
  override private[specialized] def cols$mcI$sp(m: IntMatrix): Int = m(0).length
}

object IntMatrixLib extends MatrixLib$mcI$sp {
  type Matrix = IntMatrix
  object Matrix {
    def apply(rows: Seq[Int]*): IntMatrix =
      rows.map(_.toArray).toArray
  }

  def +(m: Matrix)(n: Matrix): Matrix = {
    val sum = {
      for (row <- 0 until rows(m))
        yield {
          for (col <- 0 until cols(m))
            yield m(row)(col) + n(row)(col)
        }
    }
    Matrix(sum: _*)
  }

  def *(m: Matrix)(n: Matrix): Matrix = {
    val prod = {
      for (i <- 0 until rows(m))
        yield {
          for (j <- 0 until cols(n))
            yield {
              val mults =
                for (k <- 0 until rows(n)) yield get(m)(i)(k) * get(n)(k)(j)
              mults.fold(0)(_ + _)
            }
        }
    }
    Matrix(prod: _*)
  }
}