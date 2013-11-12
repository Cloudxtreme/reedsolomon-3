package edu.jingw.reedsolomon

import org.scalatest.FlatSpec

class GF32Spec extends FlatSpec {
  "Multiplying by the generator" should "generate every element of the field" in {
    var current = GF32.One
    for (i <- 0 until 30) {
      current *= GF32.Alpha
      assert(current !== GF32.One)
    }
    current *= GF32.Alpha
    assert(current === GF32.One)
  }

  "Dividing by the generator" should "generate every element of the field" in {
    var current = GF32.One
    for (i <- 0 until 30) {
      current /= GF32.Alpha
      assert(current !== GF32.One)
    }
    current /= GF32.Alpha
    assert(current === GF32.One)
  }

  "Adding elements" should "be done bitwise" in {
    val a = GF32(5)
    val b = GF32(13)
    assert(GF32(5 ^ 13) === a + b)
    assert(GF32(5 ^ 13) === b + a)
  }
}
