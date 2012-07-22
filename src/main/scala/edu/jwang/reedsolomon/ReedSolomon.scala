package edu.jwang.reedsolomon

import edu.jwang.algebra.Field
import edu.jwang.algebra.PolynomialRingF
import edu.jwang.algebra.EuclideanDomain

/**
 * Implements a (''n'', ''k'' = ''n'' - 2''t'') Reed-Solomon code over a finite
 * field ''F''.
 *
 * @tparam the finite field to operate over
 * @constructor Create a new RS encoder/decoder with the given parameters
 * @param alpha a primitive element of ''F''
 * @param n the length of the codewords. This must be |''F''| - 1.
 * @param t the maximum number of errors to correct
 * @param zero the additive identity of ''F''
 * @param one the multiplicative identity of ''F''
 */
class ReedSolomon[F <: Field[F]](val alpha: F, val n: Int, val t: Int, val zero: F, val one: F) {
  if (t < 0)
    throw new IllegalArgumentException("Need t >= 0")
  if ((alpha eq null) || alpha.isZero || alpha.isOne)
    throw new IllegalArgumentException("Invalid alpha")

  /** List of nonzero field elements in the form (power of alpha, element) */
  private val fieldElems = (1 until n).foldLeft(List((0, one)))(
    (elemList, i) => (i, elemList.head._2 * alpha) +: elemList)

  /** Polynomial ring over ''F'' */
  private case class PolyF(c: IndexedSeq[F]) extends PolynomialRingF[PolyF, F](c) {
    def make(coeffs: IndexedSeq[F]) = new PolyF(coeffs)
  }

  /**
   * The cyclic code generator polynomial. For RS codes, this is the product of
   * monomials (''x'' - ''α^i^'').
   */
  private val generator = {
    var current = PolyF(Vector(one))
    var alphaPower = alpha
    for (i <- 0 until 2 * t) {
      current *= PolyF(Vector(-alphaPower, one))
      alphaPower *= alpha
    }
    current
  }

  /** The amount of redundancy, 2''t''. */
  val r = 2 * t
  /** The number of message bits. */
  val k = n - r

  /**
   * Encode the given message systematically using the parameters given at
   * construction time.
   * @param msg a length ''k'' sequence of field elements
   * @return the encoded message with the original message bits at the end
   */
  def encode(msg: IndexedSeq[F]) = {
    if (msg.length != k)
      throw new IllegalArgumentException("msg length should be k")

    val m = PolyF(msg)
    val xr = PolyF(Vector(one)).shift(r, zero)

    val mxr = m * xr
    val c = mxr - mxr % generator

    c.coeffs.padTo(n, zero)
  }

  /**
   * Decode the given received symbols using the parameters given at
   * construction time, returning the message.
   * @param recvSeq a length ''n'' sequence of field elements
   * @return the decoded length ''k'' message, or `null` if decoding failed
   */
  def decode(recvSeq: IndexedSeq[F]) = {
    val dec = decodeCodeword(recvSeq)
    if (dec == null)
      null
    else
      dec.takeRight(k) // take just the message bits
  }

  /**
   * Decode the given received symbols using the parameters given at
   * construction time, returning the codeword.
   * @param recvSeq a length ''n'' sequence of field elements
   * @return the corrected length ''n'' codeword, or `null` if decoding failed
   */
  def decodeCodeword(recvSeq: IndexedSeq[F]) = {
    if (recvSeq.length != n)
      throw new IllegalArgumentException("recvSeq length should be n")

    val recv = PolyF(recvSeq)
    val syndromePoly = syndrome(recv)

    if (syndromePoly.isZero)
      recvSeq // no errors, return as is
    else {
      val xr = PolyF(Vector(one)).shift(r, zero)
      val (si, ti) = EuclideanDomain.extendedGcd(xr, syndromePoly, t)
      val ri = si * xr + ti * syndromePoly
      //println(ri)
      //println(ti)

      val omega = ri
      val lambda = ti // error locator polynomial

      val rootPairs = chienSearch(lambda)

      if (rootPairs.length != lambda.deg)
        null // decoding failure
      else {
        val locations = rootPairs map { t => (n - t._1) % n }
        //println(locations)

        val errors = forney(rootPairs, omega, lambda)

        //println(errors)

        val errorMonomials = (errors zip locations) map
          { t => PolyF(Vector(t._1)).shift(t._2, zero) }
        val errorPoly = errorMonomials.reduce(_ + _)
        val corrected = recv - errorPoly
        //println(errorPoly)
        //println(corrected)
        corrected.coeffs.padTo(n, zero)
      }
    }
  }

  /**
   * @return the syndrome polynomial
   */
  private def syndrome(recv: PolyF) = {
    var alphaPower = alpha
    var syndromes = Vector[F]()
    for (i <- 0 until r) {
      val si = recv(alphaPower)
      syndromes :+= si
      alphaPower *= alpha
    }
    PolyF(syndromes)
  }

  /**
   * @return roots of ''p''(''x'') in the form (power of alpha, element)
   */
  private def chienSearch(p: PolyF) =
    fieldElems filter { t => p(t._2).isZero }

  /**
   * @return error values
   */
  private def forney(rootPairs: List[(Int, F)], omega: PolyF, lambda: PolyF) = {
    val roots = rootPairs map { _._2 }
    val deriv = lambda.derivative
    roots map { r => -omega(r) / deriv(r) }
  }
}