package pj.domain.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}
import org.scalatest.wordspec.AnyWordSpec
import pj.domain.DomainError.IllegalIdentifier
import pj.domain.types.Identifier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pj.domain.DomainError

import scala.language.adhocExtensions

class IdentifierTest extends AnyFlatSpec with Matchers {

  "Identifier" should "be created from a non-empty string" in {
      Identifier.from("R1") match
        case Right(value) => value.identifierToString shouldBe "R1"
        case Left(_) => DomainError.IllegalIdentifier("")
  }

    "EmptyIdentifier" should "fail to create from an empty string" in {
      Identifier.from("R1") match
        case Right(_) => (Left(IllegalIdentifier("Identifier cannot be empty")))
    }
}