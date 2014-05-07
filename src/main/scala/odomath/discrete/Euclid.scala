package odomath.discrete

/**
 * User: Oleg
 * Date: 07-May-14
 * Time: 18:23
 */
object Euclid {
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

}
