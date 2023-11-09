package pj.domain.types

import pj.domain.{DomainError, Result}
import pj.domain.DomainError.IllegalNonNegativeInteger

opaque type NonNegativeInteger = Int

object NonNegativeInteger {
  def apply(number: Int): Result[NonNegativeInteger] =
    if (number >= 0) Right(number)
    else Left(IllegalNonNegativeInteger("Invalid NonNegativeInteger"))

  def lessThan(number: NonNegativeInteger, numberCompare: Int): Boolean = number < numberCompare

  extension (m: NonNegativeInteger)
    def -(n: NonNegativeInteger): Int = m - n
    def +(n: Int): NonNegativeInteger = m + n
    def NNItoInt: Int = m
    def >=(n: NonNegativeInteger): Boolean = m.NNItoInt >= n.NNItoInt


  implicit val nonNegativeIntegerOrdering: Ordering[NonNegativeInteger] =
    Ordering.fromLessThan(_ < _)
}