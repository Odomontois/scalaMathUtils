package odomath

/**
 * Created with IntelliJ IDEA.
 * User: Oleg
 * Date: 19.10.12
 * Time: 0:04
 * To change this template use File | Settings | File Templates.
 */
package object misc {
  implicit def num2MiscNum[N](num: N)(implicit algebra: Numeric[N]) = new MiscNum(num)

  implicit def num2MiscIntegral[N](num: N)(implicit algebra: Integral[N]) = new MiscIntegral(num)

  def ??? = throw new Error("implementation is missing")
}
package misc {


class MiscNum[N](num: N)(implicit algebra: Numeric[N]) {

  import algebra._

  def pow(exp: Int) = {
    def iter(iexp: Int, acc: N): N = if (iexp == exp) acc else iter(iexp + 1, acc * num)
    iter(0, one)
  }

  def thousands = num * fromInt(10).pow(3)

  def millions = num * fromInt(10).pow(6)

  def billions = num * fromInt(10).pow(9)

  def trillions = num * fromInt(10).pow(12)

  def quadrillions = num * fromInt(10).pow(15)
}

class MiscIntegral[N](num: N)(implicit algebra: Integral[N]) {

  import algebra._
  import Ordering.Implicits._

  def sqrt: N = if (num < algebra.zero) throw new ArithmeticException("negative zero")
  else if (num == algebra.zero || num == algebra.one) num
  else {
    val t = (num / fromInt(4)).sqrt * fromInt(2) + one
    if (t * t <= num) t else t - one
  }

}


}
