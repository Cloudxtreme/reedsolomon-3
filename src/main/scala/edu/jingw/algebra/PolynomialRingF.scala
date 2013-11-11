package edu.jingw.algebra

/**
 * A polynomial ring over a field ''F'', which forms a Euclidean domain.
 */
abstract class PolynomialRingF[PRF <: PolynomialRingF[PRF, F], F <: Field[F]](_coeffs: IndexedSeq[F])
  extends PolynomialRing[PRF, F](_coeffs)
  with EuclideanDomain[PRF] {

  def norm = deg

  /**
   * Divide every coefficient by a scalar.
   */
  def /(other: F) = this * other.inverse

  /**
   * Division of ``this/other`` with quotient and remainder.
   * @return ``(quotient, remainder)``
   */
  def div(other: PRF): (PRF, PRF) = {
    if (other.isZero)
      throw new ArithmeticException("/ by 0 polynomial")
    if (isZero) {
      val z = this.asInstanceOf[PRF]
      (z, z)
    } else if (deg < other.deg)
      // TODO is the cast avoidable?
      (make(IndexedSeq()), this.asInstanceOf[PRF])
    else {
      // divide other into this polynomial's highest term
      // then recurse on the rest of the terms
      val highest = coeffs.last
      val rest = coeffs.dropRight(1)

      // highest term in quotient
      val qCoeff = highest / other.coeffs.last
      val qPower = deg - other.deg
      val zero = highest - highest
      val qTerm = make(IndexedSeq(qCoeff)).shift(qPower, zero)

      // divide the rest
      val remainingPoly = this - (other * qCoeff).shift(qPower, zero)
      if (remainingPoly.deg >= deg && remainingPoly.deg != 0)
        throw new AssertionError
      val (q, r) = remainingPoly div other

      // add to the highest term
      (qTerm + q, r)
    }
  }
}
