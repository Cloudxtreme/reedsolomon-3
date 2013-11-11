package edu.jingw.reedsolomon

import org.scalacheck.Gen
import org.scalacheck.Gen.freqTuple
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

import edu.jingw.algebra.F13
import edu.jingw.algebra.Field

abstract class ReedSolomonSpecification[F <: Field[F]](
  n: Int, alpha: F, fieldGen: Gen[F], name: String)
  extends Properties("ReedSolomon" + name) {
  val zero = alpha - alpha

  private def messageGen = for {
    msgLen <- Gen.choose(0, n) if (n - msgLen) % 2 == 0
    list <- Gen.listOfN(msgLen, fieldGen)
  } yield list
  private def errorGen = for {
    errorMask <- Gen.listOfN(n, Gen.frequency(1 -> false, 3 -> true))
    errors <- Gen.listOfN(n, fieldGen)
  } yield (errorMask zip errors) map Function.tupled { (isCorrupted, error) =>
    if (isCorrupted) zero else error
  }

  /** Build an encoder appropriate for the message's length */
  private def buildEncoder(m: Seq[F]) = {
    val r = n - m.size
    assert(r % 2 == 0)
    assert(r >= 0)
    val t = r / 2
    new ReedSolomon(alpha, n, t)
  }

  property("Codewords end with message (systematic)") =
    forAll(messageGen) { m =>
      val r = n - m.size
      assert(r % 2 == 0)
      assert(r >= 0)
      val t = r / 2
      val rs = new ReedSolomon(alpha, n, t)
      val encoded = rs.encode(Vector(m: _*))
      encoded.endsWith(m) && encoded.length == m.length + t * 2 && encoded.length == n
    }

  property("Encoding and decoding without corruption is a noop") =
    forAll(messageGen) { m =>
      val rs = buildEncoder(m)
      val encoded = rs.encode(Vector(m: _*))
      val decoded = rs.decode(encoded)
      decoded match {
        case None => false // test fails automatically when decoding fails
        case Some(decodedMsg) => decodedMsg == m
      }
    }

  property("Decode can fix <= t errors") =
    forAll(messageGen, errorGen) { (m, error) =>
      val r = n - m.size
      assert(r % 2 == 0)
      assert(r >= 0)
      val t = r / 2
      val rs = new ReedSolomon(alpha, n, t)
      val encoded = rs.encode(Vector(m: _*))
      val corrupted = (encoded zip error) map Function.tupled { _ + _ }
      val decoded = rs.decode(encoded)
      val numErrors = error.count(x => x != zero)
      (numErrors <= t) ==> propBoolean(
        decoded match {
          case None => false
          case Some(decodedMsg) => decodedMsg == m
        })
    }
}

object ReedSolomonGF32Specification extends ReedSolomonSpecification[GF32](
  31,
  GF32.Alpha,
  Gen.choose(0, 31) map { GF32(_) },
  "GF32")

object ReedSolomonF13Specification extends ReedSolomonSpecification[F13](
  12,
  F13(2),
  Gen.choose(0, 12) map { F13(_) },
  "F11")
