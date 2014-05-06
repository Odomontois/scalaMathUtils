package odomath.linear

import scala.collection.immutable.IndexedSeq

/**
 * User: Oleg
 * Date: 06-May-14
 * Time: 23:17
 */
class Vector[N](private[linear] val x: IndexedSeq[N]) {
  def apply(i: Int) = x(i)

  def size = x.size

  override def toString = s"[${x mkString "\t "}]"
}

object Vector {
  def apply[N](x: N*) = new Vector[N](x toIndexedSeq)


  implicit class VectorOps[N](self: Vector[N])(implicit algebra: Numeric[N]) {

    import algebra._

    def +(other: Vector[N]) = new Vector[N](self.x zip other.x map {case (x, y) => x + y})

    def *(q: N) = new Vector[N](self.x map (_ * q))

    def unary_- = new Vector[N](self.x map negate)

    def -(other: Vector[N]) = this + (-other)

    def *(other: Vector[N]) = self.x zip other.x map {case (x, y) => x * y} reduce plus

    def *:(q: N) = this * q

    def ~(other: Vector[N]) = new Vector[N](0 to (self.size + other.size - 2) map (k =>
      Math.max(0, k - other.size + 1) to Math.min(k, self.size - 1) map (i => self(i) * other(k - i)) sum) toVector)
  }
}

