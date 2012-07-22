package edu.jwang.test.algebra

import edu.jwang.algebra.PrimeField
import edu.jwang.algebra.PolynomialRingF

case class F13(v: Int) extends PrimeField[F13](13, v) {
  def make(value: Int) = new F13(value)
}
case class F13x(c: IndexedSeq[F13]) extends PolynomialRingF[F13x, F13](c) {
  def make(coeffs: IndexedSeq[F13]) = new F13x(coeffs)
  def one = new F13x(IndexedSeq(F13(1)))
}

object F13x {
  implicit def numbersToPoly(coeffs: IndexedSeq[Int]): F13x =
    F13x(coeffs map { F13(_) })
}