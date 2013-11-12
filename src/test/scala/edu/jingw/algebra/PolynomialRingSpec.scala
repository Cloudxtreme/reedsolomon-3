package edu.jingw.algebra

import F13x.numbersToPoly
import org.scalatest.FlatSpec

class PolynomialRingSpec extends FlatSpec {
  "A polynomial" can "be differentiated" in {
    val a: F13x = Vector(2, 10, 12, 3, 10, 10)
    val deriv = a.derivative
    val expected: F13x = Vector(10, 24, 9, 40, 50)
    assert(expected === deriv)
  }

  "Extra zeros" should "be eliminated during construction" in {
    val a: F13x = Vector(1, 2, 0, 0, 0) // 1 + 2x + 0 + 0 + 0
    val expected: F13x = Vector(1, 2)
    assert(expected === a)
  }

  "Adding zero" should "do nothing" in {
    val zero: F13x = Vector()
    val f: F13x = Vector(1, 2, 3)
    assert(f === f + zero)
    assert(f === zero + f)
  }

  "Polynomials" can "be added" in {
    val a: F13x = Vector(1, 2, 3, 4)
    val b: F13x = Vector(1, 2, 3)
    val expected: F13x = Vector(2, 4, 6, 4)
    assert(expected === a + b)
    assert(expected === b + a)
  }

  they can "be evaluated" in {
    val a: F13x = Vector(1, 2, 3) // 1 + 2x + 3x^2
    // a(2) = 1 + 4 + 3*4 = 17 = 4 (mod 13)
    assert(F13(4) === a(F13(2)))
  }

  they can "be multiplied" in {
    val a: F13x = Vector(0, 10, 12, 3, 10, 10)
    val b: F13x = Vector(10, 9, 11, 12)
    val expected: F13x = Vector(0, 9, 2, 1, 2, 3, 2, 9, 3)
    assert(expected === a * b)
    assert(expected === b * a)
  }

  "Multiplying by zero" should "give zero" in {
    val zero: F13x = Vector()
    val p: F13x = Vector(10, 9, 11, 12)
    assert(zero === zero * p)
    assert(zero === p * zero)
  }

  "Polynomials" can "be converted to strings" in {
    val f: F13x = Vector(10, 9, 11, 12)
    assert("12x^3 + 11x^2 + 9x + 10" === f.toString())
  }

  "Constants" should "be converted to simple strings" in {
    val zero: F13x = Vector()
    assert("0" === zero.toString())
    val one: F13x = Vector(1)
    assert("1" === one.toString())
  }

  "Zero terms and unit coefficients" should "be dropped" in {
    val f: F13x = Vector(0, 1, 2, 0, 1, 2, 0)
    assert("2x^5 + x^4 + 2x^2 + x" === f.toString())
  }
}
