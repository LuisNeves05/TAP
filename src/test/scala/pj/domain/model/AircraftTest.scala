package pj.domain.model

import org.scalacheck.Prop.True
import org.scalatest.funsuite.AnyFunSuite

import scala.language.adhocExtensions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import pj.domain.DomainError
import pj.domain.model.Aircraft.{parseAircraft, validateAircrafts}
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}
import pj.utils.Utils.{aircraftClass, aircraftClassNumber}

import scala.::

class AircraftTest extends AnyFlatSpec with Matchers {

  "parseAircraftEmergency" should "parse valid XML into an Aircraft object" in {
    val xml =
        <aircraft id="AC1" class="1" target="139" emergency="5"/>

    Identifier.from("AC1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(139) match
          case Right(tg) => PositiveInteger(5) match
            case Right(em) => Aircraft.parseAircraft(xml) shouldBe Right(Aircraft(value, airClass, tg, Some(em)))
            case Left(_) => DomainError.IllegalNonNegativeInteger("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "parseAircraft" should "parse valid XML into an Aircraft object" in {
    val xml =
        <aircraft id="AC1" class="1" target="139"/>

    Identifier.from("AC1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(139) match
          case Right(tg) => Aircraft.parseAircraft(xml) shouldBe Right(Aircraft(value, airClass, tg, None))
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Aircraft" should "be created with valid values" in {

    Identifier.from("AC1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(139) match
          case Right(tg) => PositiveInteger(5) match
            case Right(em) =>
              val aircraft = Aircraft(value, airClass, tg, Some(em))
              aircraft.id shouldBe value
              aircraft.aircraftClassParam shouldBe airClass
              aircraft.target shouldBe tg
              aircraft.emergency shouldBe Some(em)
            case Left(_) => DomainError.IllegalNonNegativeInteger("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Validate Aircrafts" should "ids must be distinct" in {

    Identifier.from("AC1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(139) match
          case Right(tg) => PositiveInteger(5) match
            case Right(em) =>
              Identifier.from("AC2") match
                case Right(value2) => aircraftClass("2") match
                  case Right(airClass2) => NonNegativeInteger(132) match
                    case Right(tg2) => PositiveInteger(10) match
                      case Right(em2) =>
                          val aircraft = Aircraft(value, airClass, tg, Some(em))
                          val aircraft2 = Aircraft(value2, airClass2, tg2, Some(em2))
                          val aircraftList = List(aircraft, aircraft2)
                          validateAircrafts(aircraftList) shouldBe Right(true)
                      case Left(_) => DomainError.IllegalNonNegativeInteger("")
                    case Left(_) => DomainError.IllegalPositiveInteger("")
                  case Left(_) => DomainError.IllegalClassNumber("")
                case Left(_) => DomainError.IllegalIdentifier("")
            case Left(_) => DomainError.IllegalNonNegativeInteger("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Validate Aircrafts" should "with no distinct ids" in {

    Identifier.from("AC1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(139) match
          case Right(tg) => PositiveInteger(5) match
            case Right(em) =>
              Identifier.from("AC1") match
                case Right(value2) => aircraftClass("2") match
                  case Right(airClass2) => NonNegativeInteger(132) match
                    case Right(tg2) => PositiveInteger(10) match
                      case Right(em2) =>
                        val aircraft = Aircraft(value, airClass, tg, Some(em))
                        val aircraft2 = Aircraft(value2, airClass2, tg2, Some(em2))
                        val aircraftList = List(aircraft, aircraft2)
                        validateAircrafts(aircraftList) shouldBe Left(DomainError.RepeatedAircraftId("AC1"))
                      case Left(_) => DomainError.IllegalNonNegativeInteger("")
                    case Left(_) => DomainError.IllegalPositiveInteger("")
                  case Left(_) => DomainError.IllegalClassNumber("")
                case Left(_) => DomainError.IllegalIdentifier("")
            case Left(_) => DomainError.IllegalNonNegativeInteger("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }
}