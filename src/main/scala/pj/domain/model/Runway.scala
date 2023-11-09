package pj.domain.model

import pj.domain.Result
import pj.domain.model.Runway.{class_number, handles, id}
import pj.domain.types.{AircraftClass, Identifier, NonNegativeInteger, PositiveInteger}
import pj.utils.Utils.{aircraftClass, aircraftClassNumber}
import pj.xml.XML.{fromAttribute, fromNode, traverse}
import pj.domain.DomainError
import pj.domain.DomainError.IllegalIdentifier

import scala.util.Left
import scala.xml.Node

final case class Runway(id: Identifier, classes: List[AircraftClass])

object Runway:

  val id: String = "id";
  val class_number: String = "class";
  val handles: String = "handles";

  def parseRunways(xml: Node): Result[Runway] =
    for {
      id <- fromAttribute(xml, id).flatMap(Identifier.from)
      classes <- traverse((xml \\ handles).flatMap(_.attributes.find(_.key == class_number).map(_.value)), 
        classNum => aircraftClass(classNum.text))
    }yield Runway(id, classes)

  def validateRunways(la: List[Runway]): Result[Boolean] =
    la.map(_.id) match
      case ids if ids.distinct.sizeIs == la.size => Right(true)
      case ids => Left(DomainError.RepeatedRunwayId(ids.diff(ids.distinct).map(Identifier.identifierToString).mkString(", ")))
