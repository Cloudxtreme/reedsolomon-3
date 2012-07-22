package edu.jwang.algebra

import org.junit.Assert.assertEquals
import org.junit.Test

class PrimeFieldTest {
  @Test
  def testInverse() {
    val elem = new F13(4)
    assertEquals(1, (elem * elem.inverse).value)

    val elem2 = new F13(7)
    assertEquals(1, (elem2 * elem2.inverse).value)
  }
}