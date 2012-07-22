package edu.jingw.reedsolomon

import edu.jingw.algebra.Field

/**
 * Implements the Galois field ''GF''(2^5^) with irreducible polynomial
 * ''α''^5^ + ''α''^2^ + 1.
 */
case class GF32(vector: Int) extends Field[GF32] {
  if (vector < 0 || vector > 31)
    // might be better to do reduction instead
    throw new IllegalArgumentException("out of range")

  def +(other: GF32) = GF32(vector ^ other.vector)

  def unary_- = this

  def *(other: GF32) =
    if (vector == 0)
      this
    else if (other.vector == 0)
      other
    else
      GF32(GF32.powerToVector(
        (GF32.vectorToPower(vector) + GF32.vectorToPower(other.vector)) % 31))

  def inverse = {
    if (vector == 0)
      throw new ArithmeticException("/ by 0")
    GF32(GF32.powerToVector((-GF32.vectorToPower(vector) + 31) % 31))
  }

  def isOne = vector == 1
  def isZero = vector == 0

  override def toString = {
    val base = Integer.toString(vector, 2)
    val leading = "0" * (5 - base.length)
    "GF32(" + leading + base + ")"
  }
}

object GF32 {
  val Zero = new GF32(0) // binary 0
  val One = new GF32(1) // binary 1
  val Alpha = new GF32(2) // binary 10

  /** Table to convert from the vector to power representation. */
  val vectorToPower = new Array[Int](32)
  /** Table to convert from the power to vector representation. */
  val powerToVector = new Array[Int](31)

  init()

  /** Initialize the tables to convert between vectors and powers. */
  private def init() {
    // might be better to hide the array and express as method
    vectorToPower(0) = -1 // can't express 0 as power
    var vector = 1
    val alpha5 = 1 + 4 // x^5 = 1 + x^2
    for (power <- 0 until 31) {
      vectorToPower(vector) = power
      powerToVector(power) = vector

      // multiply by alpha is same as bit shifting
      vector <<= 1
      // reduce mod x^5 + x^2 + 1
      if (vector > 31)
        vector = (vector - 32) ^ alpha5
    }
  }
}