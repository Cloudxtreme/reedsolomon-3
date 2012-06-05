package edu.jwang.algebra

/**
 * A field is a commutative ring with multiplicative inverses. However, the
 * additive identity does not have a multiplicative inverse. Fields trivially
 * form a Euclidean domain with with norm 0 for every element.
 */
trait Field[F <: Field[F]] extends Ring[F]
  with EuclideanDomain[F]
  with MultiplicativeGroup[F] {

  def norm = 0

  def div(other: F) = (this / other, other - other)

  override def /(other: F) = super[MultiplicativeGroup]./(other)
}