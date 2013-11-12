package edu.jingw.algebra

import org.scalatest.FlatSpec

import F13x.numbersToPoly

class EuclideanDomainSpec extends FlatSpec {
  "The GCD of relatively prime polynomials" should "be a number" in {
    val poly1: F13x = Vector(1, 5) // 1 + 5x
    val poly2: F13x = Vector(10, 12, -1) // 10 + 12x - x^2
    val (s, t) = EuclideanDomain.extendedGcd(poly1, poly2)
    val gcd = s * poly1 + t * poly2
    // should be a constant
    assert(0 === gcd.deg)
    assert(!gcd.isZero)
  }

  "The GCD" should "find common factors" in {
    val commonPoly: F13x = Vector(10, 12, -1) // 10 + 12x - x^2
    val poly1 = commonPoly * Vector(1, 1) // multiply by 1 + x
    val poly2 = commonPoly * Vector(0, 1, 0, 3) // multiply by x + 3x^3
    val (s, t) = EuclideanDomain.extendedGcd(poly1, poly2)
    val gcd = s * poly1 + t * poly2

    // make sure gcd is multiple of poly
    val (q, r) = commonPoly div gcd
    assert(0 === q.deg)
    assert(!q.isZero)
    assert(!q.isZero)
    assert(r.isZero)
  }
}
