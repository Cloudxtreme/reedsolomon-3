package edu.jingw.algebra

import org.junit.Assert.assertEquals
import org.junit.Test

import F13x.numbersToPoly

class PolynomialRingFTest {
  @Test(expected = classOf[ArithmeticException])
  def testDivByZero() {
    val a: F13x = Vector(0, 10, 12, 3, 10, 10)
    val b: F13x = Vector()
    val (q, r) = a div b
  }

  @Test
  def testDivOfZero() {
    val a: F13x = Vector(0, 10, 12, 3, 10, 10)
    val b: F13x = Vector()
    val (q, r) = b div a
    assertEquals(q, b)
    assertEquals(r, b)
  }

  @Test
  def testDivConstant() {
    val a: F13x = Vector(6)
    val b: F13x = Vector(2)
    val (q, r) = a div b
    assertEquals(q, numbersToPoly(Vector(3)))
    assertEquals(r, numbersToPoly(Vector()))
  }

  @Test
  def testDiv() {
    val a: F13x = Vector(0, 10, 12, 3, 10, 10)
    val b: F13x = Vector(10, 9, 11, 12)
    val (q, r) = a div b
    assertEquals(q, numbersToPoly(Vector(4, 10, 3)))
    assertEquals(r, numbersToPoly(Vector(12, 4, 4)))
  }
}