package edu.jingw.algebra

/**
 * A mathematical monoid element expressed multiplicatively.
 */
trait MultiplicativeMonoid[G <: MultiplicativeMonoid[G]] {
  /**
   * Multiplication of two monoid elements. This operation must be associative.
   */
  def *(other: G): G

  /**
   * Return `true` if this is the multiplicative identity.
   */
  def isOne: Boolean
}
