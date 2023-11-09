package pj.domain.model

import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}
import pj.domain.DomainError.IllegalNonNegativeInteger
import pj.domain.{DomainError, Result}
import pj.domain.model.Agenda.{aircraft, aircrafts, runway, runways, target}
import pj.domain.model.Aircraft.{parseAircraft, validateAircrafts}
import pj.domain.model.Runway.{parseRunways, validateRunways}
import pj.utils.Utils.{aircraftClass, aircraftClassNumber}
import pj.xml.XML.{fromAttribute, fromNode, traverse}

import scala.xml.{Elem, Node}

final case class Agenda(aircrafts: List[Aircraft], runways: List[Runway], maximumDelayTime: PositiveInteger)
object Agenda {

  val aircrafts: String = "aircrafts"
  val aircraft: String = "aircraft"
  val runways: String = "runways"
  val runway: String = "runway"
  val target: String = "maximumDelayTime"


  def fromXML(xml: Node): Result[Agenda] =

    for {
      aircraftNodes <- fromNode(xml, aircrafts).flatMap(n => traverse(n \ aircraft, parseAircraft))
      _ <- validateAircrafts(aircraftNodes)
      runwayNodes <- fromNode(xml, runways).flatMap(n => traverse(n \ runway, parseRunways))
      _ <- validateRunways(runwayNodes)
      maximumDelayTime <- fromAttribute(xml, target).flatMap(a => PositiveInteger(a.toInt))
      _ <- validateAvailableRunways(runwayNodes, aircraftNodes)
    } yield Agenda(aircraftNodes, runwayNodes, maximumDelayTime)

  def validateAvailableRunways(runways: List[Runway], aircrafts: List[Aircraft]): Result[Boolean] =
    val supportedClasses = runways.flatMap(_.classes).distinct
    val unsupportedClasses = aircrafts.map(_.aircraftClassParam).filterNot(supportedClasses.contains)

    unsupportedClasses match
      case Nil => Right(true)
      case unsupported =>
        val classNumbers = unsupported.map(c => aircraftClassNumber(c).getOrElse(""))
        Left(DomainError.NoRunwaysAvailable(classNumbers.mkString(", ")))
}