package edu.jwang.algebra

/**
 * A mathematical group element expressed multiplicatively.
 */
trait MultiplicativeGroup[G <: MultiplicativeGroup[G]]
  extends MultiplicativeMonoid[G] {
  /**
   * Return the multiplicative inverse of this object.
   */
  def inverse(): G

  /**
   * Divide this element by another (compute `this/other`)
   */
  def /(other: G) = this * other.inverse

  /**
   * Multiply together ''n'' copies of this element via exponentiation by
   * squaring.
   */
  def pow(n: Int): G =
    if (n < 0)
      inverse.pow(-n)
    else if (n == 0)
      this / this.asInstanceOf[G]
    else if (n % 2 == 0) {
      val g = pow(n / 2)
      g * g
    } else
      this * pow(n - 1)
}