package edu.jwang.algebra

/**
 * A mathematical group element expressed additively.
 */
trait AdditiveGroup[G <: AdditiveGroup[G]] {
  /**
   * Addition of two group elements. This operation must be associative and
   * must be invertible via unary minus.
   */
  def +(other: G): G

  /**
   * Return the additive inverse of this object.
   */
  def unary_-(): G

  /**
   * Subtract the given element from this element (compute `this - other`).
   */
  def -(other: G) = this + -other;

  /**
   * Return `true` if this is the additive identity.
   */
  def isZero: Boolean

  /**
   * Add together ''n'' copies of this element. The default implementation is
   * exponentiation by squaring, but subclasses are free to implement more
   * efficient methods.
   */
  def additivePow(n: Int): G =
    if (n < 0)
      (-this).additivePow(-n)
    else if (n == 0)
      this - this.asInstanceOf[G]
    else if (n % 2 == 0) {
      val g = additivePow(n / 2)
      g + g
    } else
      this + additivePow(n - 1)
}