package edu.jingw.algebra

/**
 * '''F''',,2,, = {0, 1}, the smallest field
 */
abstract class BinaryField extends Field[BinaryField] {
  def unary_- = this

  /** Multiplicative identity */
  case object One extends BinaryField {
    def *(other: BinaryField) = other
    def inverse = this
    def +(other: BinaryField) = other match {
      case One => Zero
      case Zero => One
    }

    def isZero = false
    def isOne = true

    override def toString = "1"
  }

  /** Additive identity */
  case object Zero extends BinaryField {
    def *(other: BinaryField) = Zero
    def inverse = throw new ArithmeticException("/ by 0")
    def +(other: BinaryField) = other

    def isZero = true
    def isOne = false

    override def toString = "0"
  }
}