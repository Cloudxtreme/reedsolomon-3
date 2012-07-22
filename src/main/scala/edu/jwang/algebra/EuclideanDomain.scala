package edu.jwang.algebra

/**
 * A Euclidean domain is an integral domain with a norm function and division
 * algorithm.
 */
trait EuclideanDomain[D <: EuclideanDomain[D]] extends Ring[D] {
  /**
   * The norm function maps from ''D'' to the nonnegative integers and must
   * satisfy `0.norm = 0`. Further, for any ''a'', ''b'' in ''D'', there
   * exists ''q'' and ''r'' in ''D'' such that ''a'' = ''qb'' + ''r'' with
   * ''r'' = 0 or `r.norm < b.norm`.
   */
  def norm: Int

  /**
   * Compute the quotient and remainder of `this/other` in the division
   * algorithm.
   */
  def div(other: D): (D, D)

  /** The quotient in the division algorithm */
  def /(other: D) = (this div other)._1
  /** The remainder in the division algorithm */
  def %(other: D) = (this div other)._2
}

object EuclideanDomain {
  /**
   * Compute coefficients (''s'', ''t'') such that ''sa'' + ''tb'' = ''r'',
   * stopping when the norm of ''r'' is less than `stop`.
   */
  def extendedGcd[D <: EuclideanDomain[D]](a: D, b: D, stop: Int): (D, D) =
    if (a.isZero && b.isZero)
      (a, a) // (0, 0)
    else if (b.isZero)
      (a / a, b) // (1, 0)
    else if (a.isZero)
      (a, b / b) // (0, 1)
    else if (a.norm < stop)
      (a / a, a - a) // (1, 0)
    else if (b.norm < stop)
      (b / b, b - b) // (0, 1)
    else {
      val (q, r) = a div b
      if (r.norm < stop)
        (a / a, -q)
      else {
        val (s, t) = extendedGcd(b, r, stop)
        (t, s - q * t)
      }
    }

  /**
   * Compute coefficients (''s'', ''t'') such that ''sa'' + ''tb'' = ''r'',
   * where ''r'' = gcd(''a'', ''b'').
   */
  def extendedGcd[D <: EuclideanDomain[D]](a: D, b: D): (D, D) =
    if (a.isZero && b.isZero)
      (a, a) // (0, 0)
    else if (b.isZero)
      (a / a, b) // (1, 0)
    else {
      val (q, r) = a div b
      val (s, t) = extendedGcd(b, r)
      (t, s - q * t)
    }

  /**
   * Compute gcd(''a'', ''b'').
   */
  def gcd[D <: EuclideanDomain[D]](a: D, b: D): D =
    if (b.isZero)
      a
    else
      gcd(b, a % b)
}
