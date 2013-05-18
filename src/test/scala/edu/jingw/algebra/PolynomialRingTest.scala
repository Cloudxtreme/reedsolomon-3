package edu.jingw.algebra

import org.junit.Assert.assertEquals
import org.junit.Test

import F13x.numbersToPoly

class PolynomialRingTest {
  @Test
  def testDerivative() {
    val a: F13x = Vector(2, 10, 12, 3, 10, 10)
    val deriv = a.derivative
    val expected: F13x = Vector(10, 24, 9, 40, 50)
    assertEquals(expected, deriv)
  }

  @Test
  def testReduction() {
    val a: F13x = Vector(1, 2, 0, 0, 0)
    val expected: F13x = Vector(1, 2)
    assertEquals(expected, a)
  }

  @Test
  def testPlusZero() {
    val a: F13x = Vector()
    val b: F13x = Vector(1, 2, 3)
    val expected: F13x = Vector(1, 2, 3)
    assertEquals(expected, a + b)
    assertEquals(expected, b + a)
  }

  @Test
  def testPlus() {
    val a: F13x = Vector(1, 2, 3, 4)
    val b: F13x = Vector(1, 2, 3)
    val expected: F13x = Vector(2, 4, 6, 4)
    assertEquals(expected, a + b)
    assertEquals(expected, b + a)
  }

  @Test
  def testEvaluation() {
    val a: F13x = Vector(1, 2, 3)
    assertEquals(F13(4), a(F13(2)))
  }

  @Test
  def testMultiplication() {
    val a: F13x = Vector(0, 10, 12, 3, 10, 10)
    val b: F13x = Vector(10, 9, 11, 12)
    val expected: F13x = Vector(0, 9, 2, 1, 2, 3, 2, 9, 3)
    assertEquals(expected, a * b)
    assertEquals(expected, b * a)
  }

  @Test
  def testMultiplicationZero() {
    val a: F13x = Vector()
    val b: F13x = Vector(10, 9, 11, 12)
    val expected: F13x = a
    assertEquals(expected, a * b)
    assertEquals(expected, b * a)
  }

  @Test
  def testToString() {
    val f: F13x = Vector(10, 9, 11, 12)
    assertEquals("12x^3 + 11x^2 + 9x + 10", f.toString())
  }

  @Test
  def testToStringZero() {
    val f: F13x = Vector()
    assertEquals("0", f.toString())
  }

  @Test
  def testToStringOne() {
    val f: F13x = Vector(1)
    assertEquals("1", f.toString())
  }

  @Test
  def testToStringMixed() {
    val f: F13x = Vector(0, 1, 2, 0, 1, 2, 0)
    assertEquals("2x^5 + x^4 + 2x^2 + x", f.toString())
  }
}
