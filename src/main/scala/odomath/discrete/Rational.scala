package odomath.discrete

/**
 * User: Oleg
 * Date: 06-May-14
 * Time: 22:05
 */
sealed class Rational private[Rational](val num: BigInt, val den: BigInt) {
  override def toString = s"${num}/${den}"
}


object RationalOps extends Fractional[Rational] {
  override def div(x: Rational, y: Rational): Rational = Rational(x.num * y.den, y.num * x.den)

  override def toDouble(x: Rational): Double = x.num.toDouble / x.den.toDouble

  override def plus(x: Rational, y: Rational): Rational = Rational(x.num * y.den + y.num * x.den, x.den * y.den)

  override def toFloat(x: Rational): Float = x.num.toFloat / x.den.toFloat

  override def toInt(x: Rational): Int = (x.num / x.den).toInt

  override def negate(x: Rational): Rational = Rational(-x.num, x.den)

  override def fromInt(x: Int): Rational = Rational(x, 1)

  override def toLong(x: Rational): Long = (x.num / x.den).toLong

  override def times(x: Rational, y: Rational): Rational = Rational(x.num * y.num, x.den * y.den)

  override def minus(x: Rational, y: Rational): Rational = Rational(x.num * y.den - y.num * x.den, x.den * y.den)

  override def compare(x: Rational, y: Rational): Int = x.num * y.den compare y.num * x.den
}
object Rational {
  def apply(num: BigInt, den: BigInt) = {
    val gcd = num.gcd(den)
    new Rational(num / gcd, den / gcd)
  }

  implicit def fromInt(x: Int) = RationalOps.fromInt(x)
}


