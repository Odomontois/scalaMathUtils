package odomath.discrete

import Ordering.Implicits._
import Integral.Implicits._

/**
 * User: Oleg
 * Date: 07-May-14
 * Time: 18:23
 */
object Euclid {
  case class EuclidResult[N](k: N, l: N, gcd: N) {
    def swap = EuclidResult(l, k, gcd)
  }

  def euclid[N](a: N, b: N)(implicit field: Integral[N]): EuclidResult[N] = {
    import field._
    if (b == zero) EuclidResult[N](one, zero, a)
    else if (field.lt(a, b)) euclid(b, a).swap
    else {
      val (d, r) = a /% b
      val EuclidResult(k, l, gcd) = euclid(b, r)
      EuclidResult(l, minus(k, times(l, d)), gcd)
    }
  }

  def normalized[N](a: N, b: N, k: N = 1)(implicit field: Integral[N]): EuclidResult[N] = {
    def normalize(res: EuclidResult[N]): EuclidResult[N] = if (res.k > field.zero) if (k == 1 || res.k < a / res.gcd) res
    else {
      val u = (res.k * k) / (b / res.gcd)
      EuclidResult(res.k * k - (b / res.gcd) * u, res.l * k + (a / res.gcd) * u, res.gcd)
    } else normalize(EuclidResult(res.k + b / res.gcd, res.l - a / res.gcd, res.gcd))
    normalize(euclid(a, b))
  }
}
