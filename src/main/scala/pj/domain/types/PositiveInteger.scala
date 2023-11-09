package pj.domain.types

import pj.domain.Result
import pj.domain.DomainError.IllegalPositiveInteger

opaque type PositiveInteger = Int

object PositiveInteger {
  def apply(number: Int): Result[PositiveInteger] =
    if (number > 0) Right(number)
    else Left(IllegalPositiveInteger("Invalid PositiveInteger"))

  def positiveIntegerToInt(id: PositiveInteger): Int = id

  extension (m: PositiveInteger)
    def PItoInt: Int = m
    def +(n: Int): Int = m + n
}