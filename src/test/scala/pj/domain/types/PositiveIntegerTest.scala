package pj.domain.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pj.domain.DomainError
import pj.domain.DomainError.IllegalIdentifier
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}

import scala.language.adhocExtensions

class PositiveIntegerTest extends AnyFlatSpec with Matchers {

  "PositiveIntegerValid" should "be valid" in {
    PositiveInteger(5) match
        case Right(value) => value.PItoInt shouldBe 5
        case Left(_) => DomainError.IllegalNonNegativeInteger("")
  }

  "PositiveIntegerError" should "be invalid" in {
    val result = PositiveInteger(-10)
    result shouldBe Left(DomainError.IllegalPositiveInteger("Invalid PositiveInteger"))
  }
}