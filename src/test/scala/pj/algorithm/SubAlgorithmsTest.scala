package pj.algorithm

import org.scalacheck.Prop.True
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pj.domain.DomainError
import pj.domain.model.{Aircraft, Runway, ScheduleXML}
import pj.domain.model.Aircraft.{parseAircraft, validateAircrafts}
import pj.domain.schedule.ScheduleMS03.{findBestRunway, findFitForEmergency, generateAircraftsSubsets}
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}
import pj.utils.Utils.{aircraftClass, aircraftClassNumber}

import scala.::
import scala.language.adhocExtensions

class SubAlgorithmsTest extends AnyFlatSpec with Matchers {

  "Generate Aircrafts Subsets" should "must create subsets of aircrafts" in {
    Identifier.from("AC1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(10) match
          case Right(tg) => PositiveInteger(30) match
            case Right(em) =>
              Identifier.from("AC2") match
                case Right(value2) => aircraftClass("2") match
                  case Right(airClass2) => NonNegativeInteger(80) match
                    case Right(tg2) =>
                        Identifier.from("AC3") match
                          case Right(value3) => aircraftClass("2") match
                            case Right(airClass3) => NonNegativeInteger(160) match
                              case Right(tg3) =>
                                Identifier.from("AC4") match
                                  case Right(value4) => aircraftClass("2") match
                                    case Right(airClass4) => NonNegativeInteger(240) match
                                      case Right(tg4) =>
                                        val aircraft = Aircraft(value, airClass, tg, None)
                                        val aircraft2 = Aircraft(value2, airClass2, tg2, None)
                                        val aircraft3 = Aircraft(value3, airClass3, tg3, None)
                                        val aircraft4 = Aircraft(value4, airClass4, tg4, Some(em))
                                        val aircraftList = List(aircraft, aircraft2, aircraft3, aircraft4)
                                        generateAircraftsSubsets(aircraftList) shouldBe List(List(aircraft3, aircraft2, aircraft), List(aircraft4))
                                      case Left(_) => DomainError.IllegalPositiveInteger("")
                                    case Left(_) => DomainError.IllegalClassNumber("")
                                  case Left(_) => DomainError.IllegalIdentifier("")
                              case Left(_) => DomainError.IllegalPositiveInteger("")
                            case Left(_) => DomainError.IllegalClassNumber("")
                          case Left(_) => DomainError.IllegalIdentifier("")
                    case Left(_) => DomainError.IllegalPositiveInteger("")
                  case Left(_) => DomainError.IllegalClassNumber("")
                case Left(_) => DomainError.IllegalIdentifier("")
            case Left(_) => DomainError.IllegalNonNegativeInteger("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Find Best Runway" should "should find the best runway" in {
    Identifier.from("A1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(0) match
          case Right(tg) =>
              Identifier.from("R1") match
                case Right(runId) =>
                      Identifier.from("R2") match
                        case Right(runId2) =>
                          Identifier.from("A2") match
                            case Right(value2) =>
                              val aircraft = Aircraft(value, airClass, tg, None)
                              val aircraft2 = Aircraft(value2, airClass, tg, None)
                              val runway = Runway(runId, airClass :: Nil)
                              val runway2 = Runway(runId2, airClass :: Nil)
                              val listScheduleXML = List(ScheduleXML(aircraft, runway, 0, 0), ScheduleXML(aircraft2, runway2, 0, 0))
                              val runwaysAvailable = List(runway, runway2)
                              val map = runwaysAvailable.map(runway => (runway, listScheduleXML.filter(_.runway == runway)))
                                .toMap.withDefaultValue(List.empty[ScheduleXML])
                              findBestRunway(map, aircraft) shouldBe Right(runway, 82)
                            case Left(_) => DomainError.IllegalIdentifier("")
                        case Left(_) => DomainError.IllegalIdentifier("")
                case Left(_) => DomainError.IllegalIdentifier("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Find Best Runway" should "no runways available" in {
    Identifier.from("A1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(0) match
          case Right(tg) =>
            Identifier.from("R1") match
              case Right(runId) =>
                  val aircraft = Aircraft(value, airClass, tg, None)
                  val runway = Runway(runId, Nil)
                  val runwaysAvailable = List(runway)
                  val map = runwaysAvailable.map(runway => (runway, Nil))
                    .toMap.withDefaultValue(List.empty[ScheduleXML])
                  findBestRunway(map, aircraft) shouldBe Left(DomainError.NoRunwaysAvailable("1"))
              case Left(_) => DomainError.IllegalIdentifier("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Find Fit For Emergency" should "should find the best runway for the emergency" in {
    Identifier.from("A1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(0) match
          case Right(tg) =>
            Identifier.from("R1") match
              case Right(runId) =>
                Identifier.from("A2") match
                  case Right(value2) =>
                    PositiveInteger(5) match
                      case Right(em) =>
                        Identifier.from("A3") match
                          case Right(value3) =>
                            NonNegativeInteger(50) match
                              case Right(tg2) =>
                                NonNegativeInteger(90) match
                                  case Right(tg3) =>
                                    val aircraft = Aircraft(value, airClass, tg, None)
                                    val aircraft2 = Aircraft(value2, airClass, tg2, None)
                                    val aircraft3 = Aircraft(value3, airClass, tg3, Some(em))
                                    val runway = Runway(runId, airClass :: Nil)
                                    val listScheduleXML = List(ScheduleXML(aircraft, runway, 0, 0), ScheduleXML(aircraft2, runway, 0, 0))
                                    findFitForEmergency(listScheduleXML, aircraft3) shouldBe Right((runway, List(ScheduleXML(aircraft, runway, 0, 0), ScheduleXML(aircraft2, runway, 0, 0)), List(), 90))
                                  case Left(_) => DomainError.IllegalNonNegativeInteger("")
                              case Left(_) => DomainError.IllegalNonNegativeInteger("")
                          case Left(_) => DomainError.IllegalIdentifier("")
                      case Left(_) => DomainError.IllegalPositiveInteger("")
                  case Left(_) => DomainError.IllegalIdentifier("")
              case Left(_) => DomainError.IllegalIdentifier("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

  "Find Fit For Emergency" should "no runways available for emergency" in {
    Identifier.from("A1") match
      case Right(value) => aircraftClass("1") match
        case Right(airClass) => NonNegativeInteger(0) match
          case Right(tg) =>
            Identifier.from("R1") match
              case Right(runId) =>
                Identifier.from("R2") match
                  case Right(runId2) =>
                    Identifier.from("A2") match
                      case Right(value2) =>
                        PositiveInteger(5) match
                          case Right(em) =>
                            Identifier.from("A3") match
                              case Right(value3) =>
                                NonNegativeInteger(50) match
                                  case Right(tg2) =>
                                    val aircraft = Aircraft(value, airClass, tg, Some(em))
                                    val aircraft2 = Aircraft(value2, airClass, tg, Some(em))
                                    val aircraft3 = Aircraft(value3, airClass, tg2, Some(em))
                                    val runway = Runway(runId, airClass :: Nil)
                                    val runway2 = Runway(runId2, airClass :: Nil)
                                    val listScheduleXML = List(ScheduleXML(aircraft, runway, 0, 0), ScheduleXML(aircraft2, runway2, 0, 0))
                                    findFitForEmergency(listScheduleXML, aircraft3) shouldBe Left(DomainError.NoEmergencyAvailability("A3"))
                                  case Left(_) => DomainError.IllegalNonNegativeInteger("")
                              case Left(_) => DomainError.IllegalIdentifier("")
                          case Left(_) => DomainError.IllegalPositiveInteger("")
                      case Left(_) => DomainError.IllegalIdentifier("")
                  case Left(_) => DomainError.IllegalIdentifier("")
              case Left(_) => DomainError.IllegalIdentifier("")
          case Left(_) => DomainError.IllegalPositiveInteger("")
        case Left(_) => DomainError.IllegalClassNumber("")
      case Left(_) => DomainError.IllegalIdentifier("")
  }

}