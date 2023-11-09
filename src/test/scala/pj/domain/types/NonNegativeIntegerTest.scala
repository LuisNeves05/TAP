package pj.domain.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pj.domain.DomainError
import pj.domain.DomainError.IllegalIdentifier
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}

import scala.language.adhocExtensions

class NonNegativeIntegerTest extends AnyFlatSpec with Matchers {

  "NonNegative" should "be valid" in {
      NonNegativeInteger(5) match
        case Right(value) => value.NNItoInt shouldBe 5
        case Left(_) => DomainError.IllegalNonNegativeInteger("")
  }

  "NonNegativeError" should "be invalid" in {
    val result = NonNegativeInteger(-10)
    result shouldBe Left(DomainError.IllegalNonNegativeInteger("Invalid NonNegativeInteger"))
  }
}