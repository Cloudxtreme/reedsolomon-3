package edu.jingw.reedsolomon

import org.scalacheck.Gen
import org.scalacheck.Gen.freqTuple
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import edu.jingw.algebra.F13
import edu.jingw.algebra.Field

class ReedSolomonBaseSpec[F <: Field[F]](
  n: Int, alpha: F, fieldGen: Gen[F]) extends FlatSpec with GeneratorDrivenPropertyChecks {
  val zero = alpha - alpha

  private val messageGen = for {
    t <- Gen.choose(0, n / 2)
    list <- Gen.listOfN(n - 2 * t, fieldGen)
  } yield list
  private val errorGen = for {
    errorMask <- Gen.listOfN(n, Gen.frequency(20 -> false, 1 -> true))
    errors <- Gen.listOfN(n, fieldGen)
  } yield (errorMask zip errors) map {
    case (isCorrupted, error) =>
      if (isCorrupted) error else zero
  }

  /** Build an encoder appropriate for the message's length */
  private def buildEncoder(m: Seq[F]) = {
    val r = n - m.size
    assert(r % 2 == 0)
    assert(r >= 0)
    val t = r / 2
    new ReedSolomon(alpha, n, t)
  }

  "Codewords" should "end with the message (systematic)" in
    forAll(messageGen) { m =>
      val r = n - m.size
      assert(r % 2 == 0)
      assert(r >= 0)
      val t = r / 2
      val rs = new ReedSolomon(alpha, n, t)
      val encoded = rs.encode(Vector(m: _*))
      assert(encoded.endsWith(m) && encoded.length == m.length + t * 2 && encoded.length == n)
    }

  "Encoding and decoding without corruption" should "be a noop" in
    forAll(messageGen) { m =>
      val rs = buildEncoder(m)
      val encoded = rs.encode(Vector(m: _*))
      val decoded = rs.decode(encoded)
      decoded match {
        case None => fail() // test fails automatically when decoding fails
        case Some(decodedMsg) => assert(decodedMsg === m)
      }
    }

  "Decode" should "fix <= t errors" in
    forAll(messageGen, errorGen) { (m, error) =>
      val r = n - m.size
      assert(r % 2 == 0)
      assert(r >= 0)
      val t = r / 2
      val rs = new ReedSolomon(alpha, n, t)
      val encoded = rs.encode(Vector(m: _*))
      val corrupted = (encoded zip error) map Function.tupled { _ + _ }
      val decoded = rs.decode(corrupted)
      val numErrors = error.count(x => x != zero)
      whenever(numErrors <= t) {
        decoded match {
          case None => fail()
          case Some(decodedMsg) => assert(decodedMsg == m)
        }
      }
    }
}

class ReedSolomonGF32Spec extends ReedSolomonBaseSpec[GF32](
  31,
  GF32.Alpha,
  Gen.choose(0, 31) map { GF32(_) })

class ReedSolomonF13Spec extends ReedSolomonBaseSpec[F13](
  12,
  F13(2),
  Gen.choose(0, 12) map { F13(_) })
