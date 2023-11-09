package pj.domain.types

import pj.domain.DomainError.IllegalIdentifier

import scala.xml.Elem

sealed trait AircraftClass
final case class Landing(weight: Weight) extends AircraftClass
final case class TakeOff(weight: Weight) extends AircraftClass

sealed trait Weight
case object Small extends Weight
case object Large extends Weight
case object Heavy extends Weight