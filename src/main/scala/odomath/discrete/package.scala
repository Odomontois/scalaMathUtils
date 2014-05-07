package odomath

import odomath.discrete.Euclid
import Euclid.euclid
/**
 * Created with IntelliJ IDEA.
 * User: Oleg
 * Date: 21.10.12
 * Time: 20:22
 * To change this template use File | Settings | File Templates.
 */
package object discrete {


  implicit val rationalOps = RationalOps

  def gcd[N](a: N, b: N)(implicit field: Integral[N]): N = euclid(field.abs(a), field.abs(b)).gcd

}
