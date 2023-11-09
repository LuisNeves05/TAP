package pj.domain.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pj.domain.DomainError
import pj.domain.model.Agenda.validateAvailableRunways
import pj.domain.model.Runway.parseRunways
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}
import pj.utils.Utils.{aircraftClass, aircraftClassNumber}

import scala.language.adhocExtensions

class AgendaTest extends AnyFlatSpec with Matchers {

  "fromXML" should "from XML into an Agenda object" in {
    val xml =
        <agenda maximumDelayTime="900" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://www.dei.isep.ipp.pt/tap-2023" xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2023 ../../agenda.xsd">
          <aircrafts>
            <aircraft id="A1" class="1" target="1" emergency="5"/>
          </aircrafts>
          <runways>
            <runway id="R1">
              <handles class="1"/>
            </runway>
          </runways>
        </agenda>

    Identifier.from("A1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(1) match
          case Right(tg) => PositiveInteger(5) match
            case Right(em) =>
              val aircraft = Aircraft(value, airClass, tg, Some(em))
              Identifier.from("R1") match
                case Right(runId) =>
                  val runway = Runway(runId, airClass :: Nil)
                  PositiveInteger(900) match
                    case Right(delay) => Agenda.fromXML(xml) shouldBe Right(Agenda(aircraft :: Nil, runway :: Nil, delay))
                    case Left(_) => DomainError.IllegalPositiveInteger("")
                case Left(_) => DomainError.IllegalIdentifier("")
            case Left(_) => DomainError.IllegalNonNegativeInteger("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Agenda" should "be created with valid values" in {

    Identifier.from("A1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(1) match
          case Right(tg) => PositiveInteger(5) match
            case Right(em) =>
              val aircraft = Aircraft(value, airClass, tg, Some(em))
              Identifier.from("R1") match
                case Right(runId) =>
                  val runway = Runway(runId, airClass :: Nil)
                  PositiveInteger(900) match
                    case Right(delay) =>
                      val agenda = Agenda(aircraft :: Nil, runway :: Nil, delay)
                      agenda.aircrafts shouldBe aircraft :: Nil
                      agenda.runways shouldBe runway :: Nil
                      agenda.maximumDelayTime shouldBe delay
                    case Left(_) => DomainError.IllegalPositiveInteger("")
                case Left(_) => DomainError.IllegalIdentifier("")
            case Left(_) => DomainError.IllegalNonNegativeInteger("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Agenda" should "has available runways" in {

    Identifier.from("A1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(1) match
          case Right(tg) => PositiveInteger(5) match
            case Right(em) =>
              val aircraft = Aircraft(value, airClass, tg, Some(em))
              Identifier.from("R1") match
                case Right(runId) =>
                  val runway = Runway(runId, airClass :: Nil)
                      validateAvailableRunways(runway :: Nil, aircraft :: Nil) shouldBe Right(true)
                case Left(_) => DomainError.IllegalIdentifier("")
            case Left(_) => DomainError.IllegalNonNegativeInteger("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Agenda" should "doesn't have available runways" in {

    Identifier.from("A1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(1) match
          case Right(tg) => PositiveInteger(5) match
            case Right(em) => aircraftClass("2") match
              case Right(airClass2) =>
                Identifier.from("R1") match
                  case Right(runId) =>
                        val aircraft = Aircraft(value, airClass2, tg, Some(em))
                        val runway = Runway(runId, airClass :: Nil)
                        validateAvailableRunways(runway :: Nil, aircraft :: Nil) shouldBe  Left(DomainError.NoRunwaysAvailable("2"))
                  case Left(_) => DomainError.IllegalIdentifier("")
              case Left(_) => DomainError.IllegalNonNegativeInteger("")
            case Left(_) => DomainError.IllegalClassNumber("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }
}