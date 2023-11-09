package pj.domain.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pj.domain.DomainError
import pj.domain.model.Runway.parseRunways
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}
import pj.utils.Utils.{aircraftClass, aircraftClassNumber, aircraftSeparation}

import scala.language.adhocExtensions

class ScheduleXMLTest extends AnyFlatSpec with Matchers {

  "exportToXML" should "ScheduleXML export to XML" in {

    Identifier.from("A1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(1) match
          case Right(tg) => PositiveInteger(5) match
            case Right(em) =>
              val aircraft = Aircraft(value, airClass, tg, Some(em))
              Identifier.from("R1") match
                case Right(runId) =>
                  val runway = Runway(runId, airClass :: Nil)
                  val scheduleXML = ScheduleXML.exportToXML(ScheduleXML(aircraft, runway, 100, 50) :: Nil)
                  val xml = <schedule cost="50" xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2023 ../../schedule.xsd " xmlns="http://www.dei.isep.ipp.pt/tap-2023" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <aircraft id="A1" runway="R1" time="100"/>
    </schedule>
                  scheduleXML shouldBe xml
                case Left(_) => DomainError.IllegalIdentifier("")
            case Left(_) => DomainError.IllegalNonNegativeInteger("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")

  }

  "ScheduleXML" should "be created with valid values" in {

    Identifier.from("A1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(1) match
          case Right(tg) => PositiveInteger(5) match
            case Right(em) =>
              val aircraft = Aircraft(value, airClass, tg, Some(em))
              Identifier.from("R1") match
                case Right(runId) =>
                  val runway = Runway(runId, airClass :: Nil)
                  val scheduleXML = ScheduleXML(aircraft, runway, 100, 50)
                  scheduleXML.aircraft shouldBe aircraft
                  scheduleXML.runway shouldBe runway
                  scheduleXML.actualTarget shouldBe 100
                  scheduleXML.cost shouldBe 50
                case Left(_) => DomainError.IllegalIdentifier("")
            case Left(_) => DomainError.IllegalNonNegativeInteger("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")

  }
}