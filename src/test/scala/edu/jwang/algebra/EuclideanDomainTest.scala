package edu.jwang.algebra

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.assertFalse
import org.junit.Test

import F13x.numbersToPoly

class EuclideanDomainTest {
  @Test
  def testGCD() {
    val poly1: F13x = Vector(1, 5)
    val poly2: F13x = Vector(10, 12, -1)
    val (s, t) = EuclideanDomain.extendedGcd(poly1, poly2)
    val gcd = s * poly1 + t * poly2
    assertEquals(gcd.deg, 0)
    assertFalse(gcd.isZero)
  }

  @Test
  def testGCD2() {
    val poly: F13x = Vector(10, 12, -1)
    val poly1 = poly * Vector(1, 1)
    val poly2 = poly * Vector(0, 1, 0, 3)
    val (s, t) = EuclideanDomain.extendedGcd(poly1, poly2)
    val gcd = s * poly1 + t * poly2

    // make sure gcd is multiple of poly
    val (q, r) = poly div gcd
    assertEquals(0, q.deg)
    assertFalse(q.isZero)
    assertTrue(r.isZero)
  }
}