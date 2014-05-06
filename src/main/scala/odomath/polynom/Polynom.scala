package odomath.polynom

/**
 * Created with IntelliJ IDEA.
 * User: Oleg
 * Date: 07.10.12
 * Time: 13:13
 * To change this template use File | Settings | File Templates.
 */
class Polynom[T] private[Polynom](val a: Vector[Coefficient[T]])
                                 (implicit zero: DefaultZeroCoefficient[T], one: DefaultOneCoefficient[T]) {

  import Polynom._

  def +(that: Polynom[T]): Polynom[T] = Polynom(
    for (i <- 0 to math.max(this.order, that.order)) yield (if (i <= this.order) this.a(i) else zero) +
      (if (i <= that.order) that.a(i) else zero))

  def -(that: Polynom[T]): Polynom[T] = this + (-that)

  def unary_-(): Polynom[T] = Polynom(a.map(-_))

  def order = a.length - 1

  def *(that: Polynom[T]) = Polynom(
    for (t <- 0 to this.order + that.order) yield (for (i <- 0 to this.order; j = t - i if j <= that.order && j >= 0) yield this.a(i) * that.a(j)).reduceOption(_ + _).getOrElse(zero))

  def ^(p: Int): Polynom[T] =
    if (p == 0) Polynom(List(one)) else if (p == 1) this else this * (this ^ (p - 1))

  def pow(p: Int): Polynom[T] = this ^ p

  import Polynom.powerString

  override def toString = (0 to order).filter(a(_).value != zero.value).map(i => (
    (if (i > 0 && a(i).value == one.value) "" else if (i > 0 && a(i) == (- one).value) "- " else a(i)) +
      (if (i == 0) "" else if (i == 1) "x" else "x" + powerString(i.toString)))).reduce((x, y) => if (y.startsWith("-")) x + " " + y else x + " + " + y)


  def apply[P, R](x: P)(implicit map: Map[T, P, R], reduce: Reduce[R]): R =
    (0 to order).map(i => map(i, a(i).value, x)).reduceOption(reduce).getOrElse(map(0, zero.value, x))

}

object Polynom {

  type Map[T, P, R] = (Int, T, P) => R
  type Reduce[R] = (R, R) => R

  object Numeric {
    def apply[T](coef: T*)(implicit num: Numeric[T]) = new Polynom(Vector() ++ (coef.map(Coefficient(_)(num))))

    implicit def int2Polynom[T](a: Int)(implicit num: Numeric[T]) = Polynom.Numeric(num.fromInt(a))

    implicit def bd2Polynom(a: BigDecimal)(implicit num: Numeric[BigDecimal]) = Numeric(a)
  }

  object Bool {
    def apply(coef: Boolean*) = new Polynom(Vector() ++ coef.map(Coefficient(_)))

    implicit def int2Polynom(a: Int): Polynom[Boolean] = Polynom.Bool((a & 1) != 0)
  }


  def apply[T](coef: TraversableOnce[Coefficient[T]])(implicit nullCoe: DefaultZeroCoefficient[T], oneCoe: DefaultOneCoefficient[T]) = new Polynom(Vector() ++ coef)

  def x[T](implicit zero: DefaultZeroCoefficient[T], one: DefaultOneCoefficient[T]) = new Polynom(Vector(zero, one))


  var useSuperScript = false

  def powerString(s: String) = if (useSuperScript) s.map({
    case '0' => '⁰'
    case '1' => '¹'
    case '2' => '²'
    case '3' => '³'
    case '4' => '⁴'
    case '5' => '⁵'
    case '6' => '⁶'
    case '7' => '⁷'
    case '8' => '⁸'
    case '9' => '⁹'
  }).mkString
  else s
}
