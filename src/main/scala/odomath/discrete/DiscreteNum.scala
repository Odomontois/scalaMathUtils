package odomath.discrete

/**
 * User: Oleg
 * Date: 06-May-14
 * Time: 23:02
 */
abstract class DiscreteNum[N](val num: N)(implicit field: Integral[N]) {
  def -(that: DiscreteNum[N]): DiscreteNum[N]

  def +(that: DiscreteNum[N]): DiscreteNum[N]

  def *(that: DiscreteNum[N]): DiscreteNum[N]

  def /(that: DiscreteNum[N]): DiscreteNum[N]

  def fastPow(n: N): DiscreteNum[N]

  def mod(mod: N) = new DiscreteNumSpecified[N](DiscreteNum.mod(num, mod), mod)

  def modulus: Option[N]

  override def toString = num.toString
}


class DiscreteNumUnspecified[N](num: N)(implicit field: Integral[N]) extends DiscreteNum[N](num) {

  import field._

  override def modulus = None

  def ifThatMod[X](that: DiscreteNum[N], specified: DiscreteNum[N] => X, notSpecified: => X) = that.modulus match {
    case None => notSpecified
    case Some(modulus) => specified(new DiscreteNumSpecified[N](num, modulus))
  }

  override def -(that: DiscreteNum[N]) = ifThatMod(that, specified = self => self - that, notSpecified = this.num - that.num)

  override def +(that: DiscreteNum[N]) = ifThatMod(that, specified = self => self + that, notSpecified = this.num + that.num)

  override def *(that: DiscreteNum[N]) = ifThatMod(that, specified = self => self * that, notSpecified = this.num * that.num)

  override def /(that: DiscreteNum[N]) = ifThatMod(that, specified = self => self / that, notSpecified = throw new Exception(" can't divide with unspecified modulus"))

  override def fastPow(exp: N) = {
    val two = fromInt(2)
    def iter(num: N, exp: N, acc: N): N = if (exp == 0) acc else iter((num * num), exp * two, if ((exp % two) == one) (acc * num) else acc)
    new DiscreteNumUnspecified(iter(num, exp, one))
  }
}


class DiscreteNumSpecified[N] private[discrete](num: N, fieldMod: N)(implicit field: Integral[N]) extends DiscreteNum[N](num) {

  import field._
  import DiscreteNum._

  private def modul(num: N) = DiscreteNum.mod(num, fieldMod)

  private def withSameMod[X](that: DiscreteNum[N], value: => X) = {
    require(that.modulus match {
      case Some(modulus) => modulus == fieldMod
      case None => true
    }, "specified modulus " + this.modulus.get + " and " + that.modulus.get + " are different")
    value
  }

  override def modulus = Some(fieldMod)

  override def -(that: DiscreteNum[N]) = withSameMod(that, (this.num - that.num) mod fieldMod)

  override def +(that: DiscreteNum[N]) = withSameMod(that, (this.num + that.num) mod fieldMod)

  override def *(that: DiscreteNum[N]) = withSameMod(that, (this.num * that.num) mod fieldMod)

  override def /(that: DiscreteNum[N]) = withSameMod(that, {
    val EuclidResult(k, _, gcd) = euclid(that.num, fieldMod)
    if (gcd == one) (this.num * k) mod fieldMod
    else if (this.num % gcd != zero) throw new ArithmeticException("divider " + this.num + " is not divided by " + gcd + ": GCD of " + that.num + " and " + fieldMod)
    else (this.num / gcd * k) mod fieldMod
  })

  override def fastPow(exp: N) = {
    val two = fromInt(2)
    def iter(num: N, exp: N, acc: N): N = if (exp == 0) acc else iter((num * num) % fieldMod, exp / two, if ((exp % two) == one) (acc * num) % fieldMod else acc)
    new DiscreteNumSpecified(iter(num, exp, one), fieldMod)
  }

  override def toString = num.toString + " (mod " + fieldMod + ")"
}

object DiscreteNum {
  implicit def toDiscreteNum[N](num: N)(implicit field: Integral[N]):DiscreteNum[N] = new DiscreteNumUnspecified[N](num)


  def mod[N](num: N, mod: N)(implicit field: Integral[N]) = {

    import field._

    val r = rem(num, mod)
    if (lt(r, zero)) plus(r, mod) else r
  }

}