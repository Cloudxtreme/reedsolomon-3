package edu.jingw.algebra

/**
 * Polynomial ring over a ring ''R'', denoted ''R''[''x''].
 *
 * @constructor Create a new element of ''R''[''x'']
 * @param _coeffs coefficients on ''x^i^'', starting with the constant term
 */
abstract class PolynomialRing[PR <: PolynomialRing[PR, R], R <: Ring[R]](_coeffs: IndexedSeq[R])
  extends Ring[PR] with Function[R, R] with NotNull {

  /** Coefficients, excluding trailing zeros */
  val coeffs = reduce(_coeffs)

  /** Chop off trailing zeros */
  private def reduce(coeffs: IndexedSeq[R]) =
    coeffs.take(coeffs.lastIndexWhere(!_.isZero) + 1)

  /**
   * Construct a polynomial with the given coefficients. This is needed since
   * this class is abstract, but needs to make instances of itself.
   */
  protected def make(coeffs: IndexedSeq[R]): PR

  /** The degree of this polynomial. */
  def deg = if (coeffs.size == 0) 0 else coeffs.size - 1

  /**
   * Standard polynomial addition, term by term.
   */
  def +(other: PR) = {
    val common = (coeffs zip other.coeffs) map Function.tupled { _ + _ }
    val sizeDiff = coeffs.size - other.coeffs.size
    make(
      if (sizeDiff > 0)
        common ++ coeffs.takeRight(sizeDiff)
      else
        common ++ other.coeffs.takeRight(-sizeDiff))
  }

  def isOne = coeffs.size == 1 && coeffs.head.isOne
  def isZero = coeffs.size == 0

  def unary_- = make(coeffs map { -_ })

  /**
   * Standard polynomial multiplication using distributivity.
   */
  def *(other: PR) = {
    if (isZero)
      this.asInstanceOf[PR] // TODO is the cast avoidable?
    else if (other.isZero)
      other
    else {
      val finalDeg = deg + other.deg
      val resultCoeffs = for (i <- 0 to finalDeg) yield {
        // compute degree i term
        val start = math.max(0, i - other.coeffs.size + 1)
        val end = math.min(i, coeffs.size - 1) // inclusive
        // get relevant coefficients from this and other
        val c1 = coeffs.slice(start, end + 1)
        val c2 = other.coeffs.slice(i - end, i - start + 1).reverse
        // add together
        (c1 zip c2) map Function.tupled { _ * _ } reduce { _ + _ }
      }
      make(resultCoeffs)
    }
  }

  /**
   * Multiplication all coefficients by the given scalar.
   */
  def *(other: R) = make(coeffs map { _ * other })

  /**
   * Evaluation at some ''x'' in the base ring using Horner's rule.
   */
  def apply(x: R) =
    if (coeffs.isEmpty) x - x
    else coeffs.reduceRight((ai, fx) => ai + x * fx)

  /**
   * Multiply by ''x''^`amt`^ and fill in the leading coefficients with ''z''
   */
  def shift(amt: Int, z: R) = make(Vector.fill(amt)(z) ++ coeffs)

  /**
   * The formal derivative of this polynomial.
   */
  def derivative = make(coeffs.drop(1).zipWithIndex map
    Function.tupled { (a, n) => a.additivePow(n + 1) })

  /**
   * Pretty print this polynomial.
   */
  override def toString() = {
    val xPowStr = (0 to coeffs.size) map { i =>
      if (i == 0) "" else if (i == 1) "x" else "x^" + i
    }
    val rStr = coeffs map { r =>
      if (r.isZero) None
      else if (r.isOne) Some("")
      else Some(r.toString)
    }
    val combined = rStr zip xPowStr flatMap {
      case (None, x) => None
      case (Some(r), x) => Some(
        if (r == "" && x == "") "1"
        else r + x
      )
    }

    if (combined.size == 0)
      "0"
    else
      combined.reverse.mkString(" + ")
  }

  /**
   * Two polynomials are equal iff their coefficients are equal. This method
   * relies on the ``equals`` method of the underlying
   * [[scala.collection.IndexedSeq]] and the ``equals`` method of the
   * underlying ring ''R''.
   */
  override def equals(that: Any) = that match {
    case other: PR => coeffs == other.coeffs
    case _ => false
  }

  override def hashCode() = coeffs.hashCode()
}
