package edu.jingw.algebra

/**
 * The field of integers modulo ''p'', where ''p'' must be prime.
 *
 * @constructor Create a new field element in '''F''',,`modulus`,,.
 * @param modulus the modulus, which must be prime
 * @param _value integer value, which will be reduced mod `modulus`
 */
abstract class PrimeField[F <: PrimeField[F]](modulus: Int, _value: Int)
  extends Field[F] with NotNull {
  val value = ((_value % modulus) + modulus) % modulus

  def make(value: Int): F

  def inverse = make(extendedGcd(value, modulus)._1)

  def +(other: F) = make(value + other.value)

  def unary_- = make(-value)

  def *(other: F) = make(value * other.value)

  def isOne = value == 1
  def isZero = value == 0

  override def toString = value.toString
  override def equals(that: Any) = that match {
    case other: F => value == other.value
    case _ => false
  }

  private def extendedGcd(a: Int, b: Int): (Int, Int) =
    if (b == 0)
      (1, 0)
    else {
      val (q, r) = (a / b, a % b)
      val (s, t) = extendedGcd(b, r)
      (t, s - q * t)
    }
}
