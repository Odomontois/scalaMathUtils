package odomath.linear

import scala.collection.immutable.IndexedSeq
import odomath.linear.Vector._

/**
 * User: Oleg
 * Date: 06-May-14
 * Time: 23:17
 */
class Matrix[N](private[linear] val v: IndexedSeq[Vector[N]]) {
  def apply(i: Int) = v(i)

  override def toString = v mkString "\n"

  def size = v.size

  def t = new Matrix[N](0 until this(0).size map (i => new Vector[N](0 until size map (j => this(j)(i)))))
}

object Matrix {
  def identity[N](n: Int)(implicit algebra: Numeric[N]) = new Matrix[N](0 until n map (i => new Vector[N](0 until n map (j => if (i == j) algebra.one else algebra.zero))))

  def apply[N](v: Seq[N]*) = new Matrix[N](v map (x => new Vector[N](x toIndexedSeq)) toIndexedSeq)

  implicit class MatrixOps[N](self: Matrix[N])(implicit algebra: Fractional[N]) {

    import algebra._

    def *(vector: Vector[N]) = new Vector(self.v map (vector * _))

    def *(other: Matrix[N]) = {
      val trans = other.t
      new Matrix[N](0 until self.size map (i => new Vector[N](0 until other.size map (j => self(i) * trans(j)))))
    }

    def invert = {
      implicit object DualOps extends Fractional[(N, N)] {

        import Numeric.Implicits.infixNumericOps
        import Ordering.Implicits.infixOrderingOps

        def plus(x: (N, N), y: (N, N)): (N, N) = (x._1 + y._1, x._2 + y._2)

        def minus(x: (N, N), y: (N, N)): (N, N) = (x._1 - y._1, x._2 - y._2)

        def times(x: (N, N), y: (N, N)): (N, N) = (x._1 * y._1, x._2 * y._1)

        def negate(x: (N, N)): (N, N) = (-x._1, -x._2)

        def fromInt(x: Int): (N, N) = (algebra.fromInt(x), algebra.fromInt(x))

        def toInt(x: (N, N)): Int = algebra.toInt(x._1)

        def toLong(x: (N, N)): Long = algebra.toLong(x._1)

        def toFloat(x: (N, N)): Float = algebra.toFloat(x._1)

        def toDouble(x: (N, N)): Double = algebra.toDouble(x._1)

        def compare(x: (N, N), y: (N, N)): Int = if (x._1 < y._1) -1 else if (x._1 > y._1) 1 else if (x._2 < y._2) -1 else if (x._2 > y._2) 1 else 0

        def div(x: (N, N), y: (N, N)): (N, N) = (algebra.div(x._1, y._1), algebra.div(x._2, y._1))
      }

      def iter(dual: Matrix[(N, N)], step: Int): Option[Matrix[N]] = if (step == self.size) Some(new Matrix[N](dual.v map (v => new Vector[N](v.x map (_._2)))))
      else if (step until self.size forall (dual(_)(step)._1 == zero)) None
      else {
        import scala.math.Fractional.Implicits._
        val line = if (dual(step)(step)._1 != zero) dual(step)
        else dual(step) + dual.v.find(_(step)._1 != zero).get
        val result = new Matrix[(N, N)](((0 until self.size map (i => if (i == step) (line * (DualOps.one / line(step))) else dual(i) - line * (dual(i)(step) / line(step))))))
        iter(result, step + 1)
      }
      iter(new Matrix[(N, N)](0 until self.size map (i => new Vector[(N, N)](0 until self.size map (j => (self(i)(j), if (i == j) one else zero))))), 0)
    }

    def unary_! = invert.get
  }
}

