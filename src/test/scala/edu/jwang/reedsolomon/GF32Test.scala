package edu.jwang.reedsolomon

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test

class GF32Test {
  @Test
  def testGeneratorOrder() {
    var current = GF32.One
    for (i <- 0 until 30) {
      current *= GF32.Alpha
      assertFalse(current equals GF32.One)
    }
    current *= GF32.Alpha
    assertEquals(current, GF32.One)
  }

  @Test
  def testGeneratorOrderDiv() {
    var current = GF32.One
    for (i <- 0 until 30) {
      current /= GF32.Alpha
      assertFalse(current equals GF32.One)
    }
    current /= GF32.Alpha
    assertEquals(current, GF32.One)
  }

  @Test
  def testPlus() {
    val a = GF32(5)
    val b = GF32(13)
    assertEquals(GF32(5 ^ 13), a + b)
    assertEquals(GF32(5 ^ 13), b + a)
  }
}