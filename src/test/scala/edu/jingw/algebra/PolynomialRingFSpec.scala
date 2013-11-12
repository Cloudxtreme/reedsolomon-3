package edu.jingw.algebra

import org.scalatest.FlatSpec

import F13x.numbersToPoly

class PolynomialRingFSpec extends FlatSpec {
  "Dividing by zero" should "throw an exception" in {
    intercept[ArithmeticException] {
      val a: F13x = Vector(0, 10, 12, 3, 10, 10)
      val zero: F13x = Vector()
      val (q, r) = a div zero
    }
  }

  "Dividing zero by something" should "give zero" in {
    val a: F13x = Vector(0, 10, 12, 3, 10, 10)
    val zero: F13x = Vector()
    val (q, r) = zero div a
    assert(q === zero)
    assert(r === zero)
  }

  "Dividing constants" should "be the same as regular division" in {
    val six: F13x = Vector(6)
    val two: F13x = Vector(2)
    val (q, r) = six div two
    assert(q === numbersToPoly(Vector(3)))
    assert(r === numbersToPoly(Vector()))
  }

  "Dividing polynomials" should "give the quotient and remainder" in {
    val a: F13x = Vector(0, 10, 12, 3, 10, 10)
    val b: F13x = Vector(10, 9, 11, 12)
    val (q, r) = a div b
    assert(q === numbersToPoly(Vector(4, 10, 3)))
    assert(r === numbersToPoly(Vector(12, 4, 4)))
  }
}
