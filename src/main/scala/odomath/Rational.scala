package odomath

import runtime.ScalaRunTime

/**
 * User: admin
 * Date: 08.02.13
 * Time: 0:41
 */
class Rational[N](val num: N, val den: N)(implicit algebra: Integral[N]) {
  override def toString: String = s"Rat(${num}/${den})"

  override def equals(obj: Any): Boolean = obj match {
    case Rational(num, den) => num == this.num && den == this.den
    case _                  => false
  }

  override def hashCode(): Int = ScalaRunTime._hashCode((num, den))

  override def clone(): AnyRef = Rational(num, den)
}

object Rational {
  def apply[N](num: N, den: N)(implicit algebra: Integral[N]) = {
    import algebra._
    val gcd = discrete.gcd(num, den)
    if (den < zero) new Rational(-num / gcd, -den / gcd) else new Rational(num / gcd, den / gcd)
  }

  def unapply[N](rat: Rational[N]): Option[(N, N)] = Some(rat.num, rat.den)

  implicit def rational2Fractional[N](implicit algebra: Integral[N]) = new RationalAsIntegral[N] with Fractional[Rational[N]]

  class RationalAsIntegral[N](implicit algebra: Integral[N]) {

    import Numeric.Implicits._

    def div(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.num * y.den, x.den * y.num)

    def plus(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.num * y.den + y.num * x.den, x.den * y.den)

    def minus(x: Rational[N], y: Rational[N]): Rational[N] = plus(x, negate(y))

    def times(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.num * y.num, x.den * y.den)

    def negate(x: Rational[N]): Rational[N] = Rational[N](algebra.negate(x.num), x.den)

    def fromInt(x: Int): Rational[N] = Rational(algebra.fromInt(x), algebra.fromInt(1))

    def toInt(x: Rational[N]): Int = algebra.toInt(x.num) / algebra.toInt(x.den)

    def toLong(x: Rational[N]): Long = algebra.toLong(x.num) / algebra.toLong(x.den)

    def toFloat(x: Rational[N]): Float = algebra.toFloat(x.num) / algebra.toFloat(x.den)

    def toDouble(x: Rational[N]): Double = algebra.toDouble(x.num) / algebra.toDouble(x.den)

    def compare(x: Rational[N], y: Rational[N]): Int = algebra.compare(x.num * y.den, y.num * x.den)

  }

}
