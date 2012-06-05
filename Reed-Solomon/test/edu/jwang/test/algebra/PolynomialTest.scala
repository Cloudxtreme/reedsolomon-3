package edu.jwang.test.algebra

import org.junit.Assert.assertEquals
import org.junit.Test

import edu.jwang.algebra.EuclideanDomain
import F13x.numbersToPoly

class PolynomialTest {
  @Test
  def testDiv() {
    val a: F13x = Vector(0, 10, 12, 3, 10, 10)
    val b: F13x = Vector(10, 9, 11, 12)
    val (q, r) = a div b
    assertEquals(q, numbersToPoly(Vector(4, 10, 3)))
    assertEquals(r, numbersToPoly(Vector(12, 4, 4)))
  }

  @Test
  def testDerivative() {
    val a: F13x = Vector(2, 10, 12, 3, 10, 10)
    val deriv = a.derivative
    val expected: F13x = Vector(10, 24, 9, 40, 50)
    assertEquals(expected, deriv)
  }
}