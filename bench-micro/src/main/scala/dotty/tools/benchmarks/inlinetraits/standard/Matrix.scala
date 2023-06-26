package dotty.tools.benchmarks.inlinetraits
package standard

import scala.reflect.ClassTag

trait MatrixLib[T: ClassTag]:
  opaque type Matrix = Array[Array[T]]

  object Matrix:
    def apply(rows: Seq[T]*): Matrix =
      rows.map(_.toArray).toArray

  extension (m: Matrix)
    def apply(x: Int)(y: Int): T = m(x)(y)
    def rows: Int = m.length
    def cols: Int = m(0).length

object IntMatrixLib extends MatrixLib[Int]:
  extension (m: Matrix)
    def +(n: Matrix): Matrix =
      val sum =
        for row <- 0 until m.rows
        yield
          for col <- 0 until m.cols
          yield m(row)(col) + n(row)(col)
      Matrix(sum*)
    end +

    def *(n: Matrix): Matrix =
      val prod =
        for i <- 0 until m.rows
        yield
          for j <- 0 until n.cols
          yield
            val mults = for k <- 0 until n.rows yield m(i)(k) * n(k)(j)
            mults.fold(0)(_ + _)
      Matrix(prod*)
    end *