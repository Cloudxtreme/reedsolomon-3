package edu.jingw.algebra

/**
 * A ring is an abelian group under addition and a monoid under multiplication.
 * Furthermore, multiplication must distribute over addition.
 */
trait Ring[R <: Ring[R]] extends AdditiveGroup[R] with MultiplicativeMonoid[R]
