package pj.domain.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pj.domain.DomainError
import pj.domain.model.Runway.{parseRunways, validateRunways}
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}
import pj.utils.Utils.{aircraftClass, aircraftClassNumber}

import scala.language.adhocExtensions

class RunwayTest extends AnyFlatSpec with Matchers {

  "parseRunway" should "parse valid XML into a Runway object" in {
    val xml =
      <runway id="R1">
        <handles class="1"/>
        <handles class="2"/>
      </runway>

    Identifier.from("R1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => aircraftClass("2") match
          case Right(tg) => parseRunways(xml) shouldBe Right(Runway(value, airClass :: tg :: Nil))
          case Left(_) => DomainError.IllegalClassNumber("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Runway" should "be created with valid values" in {

    Identifier.from("R1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => aircraftClass("2") match
          case Right(tg) =>
            val runway = Runway(value, airClass :: tg :: Nil)
            runway.id shouldBe value
            runway.classes shouldBe airClass :: tg :: Nil
          case Left(_) => DomainError.IllegalClassNumber("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "ValidateRunways" should "with distinct ids" in {

    Identifier.from("R1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => aircraftClass("2") match
          case Right(tg) =>
            Identifier.from("R2") match
              case Right(value2) => aircraftClass("2") match
                case Right(airClass2) => aircraftClass("3") match
                  case Right(tg2) =>
                      val runway = Runway(value, airClass :: tg :: Nil)
                      val runway2 = Runway(value2, airClass2 :: tg2 :: Nil)
                      val runways = List(runway, runway2)
                      validateRunways(runways) shouldBe Right(true)
                  case Left(_) => DomainError.IllegalClassNumber("")
                case Left(_) => DomainError.IllegalClassNumber("")
              case Left(_) => DomainError.IllegalIdentifier("")
          case Left(_) => DomainError.IllegalClassNumber("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "ValidateRunways" should "with no distinct ids" in {

    Identifier.from("R1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => aircraftClass("2") match
          case Right(tg) =>
            Identifier.from("R1") match
              case Right(value2) => aircraftClass("2") match
                case Right(airClass2) => aircraftClass("3") match
                  case Right(tg2) =>
                    val runway = Runway(value, airClass :: tg :: Nil)
                    val runway2 = Runway(value2, airClass2 :: tg2 :: Nil)
                    val runways = List(runway, runway2)
                    validateRunways(runways) shouldBe Left(DomainError.RepeatedRunwayId("R1"))
                  case Left(_) => DomainError.IllegalClassNumber("")
                case Left(_) => DomainError.IllegalClassNumber("")
              case Left(_) => DomainError.IllegalIdentifier("")
          case Left(_) => DomainError.IllegalClassNumber("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }
}