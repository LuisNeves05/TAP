package pj.domain.model

import pj.domain.DomainError.IllegalIdentifier
import pj.domain.model.Aircraft.{class_number, emergency, id, target_time}
import pj.domain.{DomainError, Result, model}
import pj.domain.types.AircraftClass
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}
import pj.utils.Utils.aircraftClass
import pj.xml.XML.fromNode
import pj.xml.XML.traverse
import pj.xml.XML.fromAttribute

import scala.xml.{Elem, Node}

final case class Aircraft(id: Identifier, aircraftClassParam: AircraftClass, target: NonNegativeInteger, emergency: Option[PositiveInteger])

object Aircraft:

  val id: String = "id";
  val class_number: String = "class";
  val target_time: String = "target";
  val emergency: String = "@emergency"
  def parseAircraft(xml: Node): Result[Aircraft] =
  
    for {
      id <- fromAttribute(xml, id).flatMap(Identifier.from)
      cls <- fromAttribute(xml, class_number).flatMap(aircraftClass)
      target <- fromAttribute(xml, target_time).flatMap(a => NonNegativeInteger(a.toInt))
      emergency <- (xml \ emergency).headOption match
        case Some(emergencyValue) => PositiveInteger(emergencyValue.text.toInt).map(Some.apply)
        case None => Right(None)
    } yield Aircraft(id, cls, target, emergency)

  def validateAircrafts(la: List[Aircraft]): Result[Boolean] =
    la.map(_.id) match
      case ids if ids.distinct.sizeIs == ids.size => Right(true)
      case ids => Left(DomainError.RepeatedAircraftId(ids.diff(ids.distinct).map(Identifier.identifierToString).mkString(", ")))