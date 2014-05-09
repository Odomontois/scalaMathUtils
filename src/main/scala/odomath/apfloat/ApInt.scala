package odomath.apfloat

import org.apfloat.Apint

/**
 * User: Oleg
 * Date: 09-May-14
 * Time: 13:39
 */
object ApInt {
  def apply(x: Int) = new Apint(x)

  def apply(x: Long) = new Apint(x)

  def apply(x: Short) = new Apint(x)

  def apply(x: Byte) = new Apint(x)

  def apply(x: String) = new Apint(x)

  def apply(x: String, radix: Int) = new Apint(x, radix)

  implicit object ApIntIntegral extends Integral[Apint] {
    override def quot(x: Apint, y: Apint): Apint = x divide y

    override def rem(x: Apint, y: Apint): Apint = x mod y

    override def toDouble(x: Apint): Double = x.doubleValue

    override def plus(x: Apint, y: Apint): Apint = x add y

    override def toFloat(x: Apint): Float = x.floatValue

    override def toInt(x: Apint): Int = x.intValue

    override def negate(x: Apint): Apint = x.negate

    override def fromInt(x: Int): Apint = new Apint(x)

    override def toLong(x: Apint): Long = x.longValue

    override def times(x: Apint, y: Apint): Apint = x multiply y

    override def minus(x: Apint, y: Apint): Apint = x subtract y

    override def compare(x: Apint, y: Apint): Int = x compareTo y
  }
}
