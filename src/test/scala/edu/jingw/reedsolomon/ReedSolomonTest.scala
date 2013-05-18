package edu.jingw.reedsolomon

import org.junit.Assert.assertEquals
import org.junit.Test

import edu.jingw.algebra.PrimeField

class ReedSolomonTest {
  case class F11(v: Int) extends PrimeField[F11](11, v) {
    def make(value: Int) = new F11(value)
  }

  implicit def strToGF(s: String) = GF32(Integer.parseInt(s, 2))
  implicit def intToF11(i: Int) = F11(i)

  @Test
  def testNoError() {
    val alpha = F11(2)
    val rs = new ReedSolomon(alpha, 10, 2)

    val msg = Vector[F11](2, 1, 0, 4, 0, 3)
    val codeword = rs.encode(msg)
    val expected = Vector[F11](3, 7, 6, 10, 2, 1, 0, 4, 0, 3)
    assertEquals(expected, codeword)

    val decoded = rs.decode(codeword)
    assertEquals(Some(msg), decoded)
  }

  @Test
  def testF11() {
    val alpha = F11(2)
    val rs = new ReedSolomon(alpha, 10, 2)

    val msg = Vector[F11](2, 1, 0, 4, 0, 3)
    val originalCodeword = rs.encode(msg)
    val expected = Vector[F11](3, 7, 6, 10, 2, 1, 0, 4, 0, 3)
    assertEquals(expected, originalCodeword)

    val errors = Vector[F11](10, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    val corruptedCodeword = (originalCodeword zip errors) map Function.tupled { _ + _ }

    val decoded = rs.decode(corruptedCodeword)
    assertEquals(Some(msg), decoded)
  }

  @Test
  def testGF32() {
    val msg = Vector[GF32](
      "00101", "00010", "10101", "11000", "10001",
      "10110", "10101", "10101", "00001", "10110",
      "00000", "10111", "10100", "01111", "00110",
      "00111", "11110", "00001", "00110", "00001",
      "01011", "10000", "01100", "01101", "00100")
    val errors = Vector[GF32]("01100").padTo(31, GF32.Zero)
    val rs = new ReedSolomon(GF32.Alpha, 31, 3)
    val codeword = rs.encode(msg)
    val corruptedCodeword = (codeword zip errors) map Function.tupled { _ + _ }
    val decodedMsg = rs.decode(corruptedCodeword)
    assertEquals(Some(msg), decodedMsg)
  }
}