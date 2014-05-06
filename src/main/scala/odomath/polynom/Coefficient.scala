package odomath.polynom


trait Coefficient[T] {
  def value: T

  def +(that: Coefficient[T]): Coefficient[T]

  def *(that: Coefficient[T]): Coefficient[T]

  def unary_-(): Coefficient[T]
}

trait DefaultZeroCoefficient[T] extends Coefficient[T]

trait DefaultOneCoefficient[T] extends Coefficient[T]


object Coefficient {

  import Numeric.Implicits._

  class NumericCoefficient[T](override val value: T)(implicit num: Numeric[T]) extends Coefficient[T] {

    def +(that: Coefficient[T]) = Coefficient(this.value + that.value)

    def *(that: Coefficient[T]) = Coefficient(this.value * that.value)

    def unary_-() = Coefficient(-value)

    override def toString = if (num.lt(value, num.zero)) "- " + num.abs(value) else value.toString
  }

  class BoolCoefficient(override val value: Boolean) extends Coefficient[Boolean] {
    def +(that: Coefficient[Boolean]) = Coefficient(value ^ that.value)

    def *(that: Coefficient[Boolean]) = Coefficient(value && that.value)

    def unary_-() = this

    override def toString = if (value) "1" else "0"
  }

  def apply[T](coe: T)(implicit num: Numeric[T]): Coefficient[T] = new NumericCoefficient[T](coe)

  def apply(coe: Boolean): Coefficient[Boolean] = new BoolCoefficient(coe)

  implicit def implicitNumericZero[T](implicit num: Numeric[T]) = new NumericCoefficient(num.zero) with DefaultZeroCoefficient[T]

  implicit def implicitNumericOne[T](implicit num: Numeric[T]) = new NumericCoefficient(num.one) with DefaultOneCoefficient[T]

  implicit val implicitBooleanOne = new BoolCoefficient(true) with DefaultOneCoefficient[Boolean]
  implicit val implicitBooleanZero = new BoolCoefficient(false) with DefaultZeroCoefficient[Boolean]


  //  implicit val longNull = implicitZero[Long]
  //  implicit val longOne = implicitOne[Long]
}