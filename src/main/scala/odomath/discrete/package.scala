package odomath

/**
 * Created with IntelliJ IDEA.
 * User: Oleg
 * Date: 21.10.12
 * Time: 20:22
 * To change this template use File | Settings | File Templates.
 */
package object discrete {

  implicit val rationalOps = RationalOps

  case class EuclidResult[N](k: N, l: N, gcd: N)(implicit field: Integral[N]) {
    def swap = EuclidResult(l, k, gcd)
  }

  def euclid[N](a: N, b: N)(implicit field: Integral[N]): EuclidResult[N] = {
    if (b == field.zero) EuclidResult[N](field.one, field.zero, a)
    else if (field.lt(a, b)) euclid(b, a).swap
    else {
      import field._
      val d = quot(a, b)
      val r = rem(a, b)
      val EuclidResult(k, l, gcd) = euclid(b, r)
      EuclidResult(l, minus(k, times(l, d)), gcd)
    }
  }

  def gcd[N](a: N, b: N)(implicit field: Integral[N]): N = euclid(field.abs(a), field.abs(b)).gcd

}
