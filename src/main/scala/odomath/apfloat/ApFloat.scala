package odomath.apfloat

import org.apfloat.{Apcomplex, Apfloat}

/**
 * User: Oleg
 * Date: 09-May-14
 * Time: 13:33
 */
object ApFloat {
  def apply(x: Int) = new Apfloat(x)

  def apply(x: Long) = new Apfloat(x)

  def apply(x: Float) = new Apfloat(x)

  def apply(x: Double) = new Apfloat(x)

  def apply(x: String) = new Apfloat(x)

  def apply(x: String, radix: Int) = new Apfloat(x, Apcomplex.DEFAULT, radix)

  implicit object ApFloatFractional extends Fractional[Apfloat] {
    override def div(x: Apfloat, y: Apfloat): Apfloat = x divide y

    override def toDouble(x: Apfloat): Double = x.doubleValue

    override def plus(x: Apfloat, y: Apfloat): Apfloat = x add y

    override def toFloat(x: Apfloat): Float = x.floatValue

    override def toInt(x: Apfloat): Int = x.intValue

    override def negate(x: Apfloat): Apfloat = x.negate

    override def fromInt(x: Int): Apfloat = new Apfloat(x)

    override def toLong(x: Apfloat): Long = x.longValue

    override def times(x: Apfloat, y: Apfloat): Apfloat = x multiply y

    override def minus(x: Apfloat, y: Apfloat): Apfloat = x subtract y

    override def compare(x: Apfloat, y: Apfloat): Int = x compareTo y
  }

}
