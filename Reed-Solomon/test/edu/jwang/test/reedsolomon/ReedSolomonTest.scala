package edu.jwang.test.reedsolomon

import edu.jwang.algebra.PrimeField
import edu.jwang.algebra.PolynomialRingF
import org.junit.Test
import org.junit.Assert._
import edu.jwang.reedsolomon.ReedSolomon
import edu.jwang.reedsolomon.GF32

class ReedSolomonTest {
  case class F11(v: Int) extends PrimeField[F11](11, v) {
    def make(value: Int) = new F11(value)

    override def equals(other: Any) = true
  }

  @Test
  def testRS() {
    val alpha = F11(2)
    val rs = new ReedSolomon(alpha, 10, 2, F11(0), F11(1))

    val msg = Vector(2, 1, 0, 4, 0, 3) map { F11(_) }
    val originalCodeword = rs.encode(msg)
    val expected = Vector(10, 4, 3, 2, 2, 1, 0, 0, 0, 0) map { F11(_) }
    assertEquals(expected, originalCodeword)

    val fakeErrors = Vector(10, 0, 0, 0, 0, 0, 0, 0, 0, 1) map { F11(_) }
    val corruptedCodeword = (originalCodeword zip fakeErrors) map { t => t._1 + t._2 }

    //println(corruptedCodeword)

    val decoded = rs.decode(corruptedCodeword)
    assertEquals(msg, decoded)
  }

  @Test
  def testRS2() {
    implicit def strToGF(s: String) = GF32(Integer.parseInt(s, 2))

    val msg = Vector[GF32]("00101", "00010", "10101", "11000", "10001", "10110", "10101", "10101", "00001", "10110", "00000", "10111", "10100", "01111", "00110", "00111", "11110", "00001", "00110", "00001", "01011", "10000", "01100", "01101", "00100")
    val errors = Vector[GF32]("01100").padTo(31, GF32.Zero)
    val rs = new ReedSolomon(GF32.Alpha, 31, 3, GF32.Zero, GF32.One)
    val codeword = rs.encode(msg)
    val corruptedCodeword = (codeword zip errors) map { t => t._1 + t._2 }
    val decodedMsg = rs.decode(corruptedCodeword)
    assertEquals(msg, decodedMsg)
  }
}