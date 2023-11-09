package pj.domain.model.property

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import pj.domain.model.property.PropertyGenBase.genAgenda
import pj.domain.{DomainError, Result}
import pj.domain.model.{Agenda, Aircraft, Runway, ScheduleXML}
import pj.domain.schedule.ScheduleMS01
import pj.domain.types.{AircraftClass, Heavy, Identifier, Landing, Large, NonNegativeInteger, PositiveInteger, Small, TakeOff, Weight}
import pj.utils.Utils.aircraftClass
import pj.utils.Utils.{aircraftClass, aircraftClassNumber, aircraftSeparation, delayPenalty}
import org.scalacheck.Properties

object PropertyBasedTest extends Properties("Aircraft") :

  property("In a valid schedule, every aircraft was assigned a runway") = forAll(genAgenda)(la => {
    ScheduleMS01.assignRunway(la).fold({
      case DomainError.RepeatedAircraftId(_) => true
      case DomainError.RepeatedRunwayId(_) => true
      case DomainError.NoRunwaysAvailable(_) => true
      case DomainError.MaximumTimeWindowExceeded(_) => true
      case _ => false
    }, scheduleXML => {
      la.aircrafts.forall(aircraft => scheduleXML.exists(_.aircraft == aircraft))
    })
  })

  property("Each aircraft should be scheduled for a runway which can handle it") = forAll(genAgenda)(la => {
    ScheduleMS01.assignRunway(la).fold({
      case DomainError.RepeatedAircraftId(_) => true
      case DomainError.RepeatedRunwayId(_) => true
      case DomainError.NoRunwaysAvailable(_) => true
      case DomainError.MaximumTimeWindowExceeded(_) => true
      case _ => false
    }, scheduleXML => {
      scheduleXML.forall { schedule =>
        val aircraftId = schedule.aircraft.id
        val runwayId = schedule.runway.id
        val aircraftOpt = la.aircrafts.find(_.id == aircraftId)
        val runwayOpt = la.runways.find(_.id == runwayId)

        (aircraftOpt, runwayOpt) match {
          case (Some(aircraft), Some(runway)) =>
            val aircraftClassCode = aircraft.aircraftClassParam
            val acceptedClassCodes = runway.classes
            acceptedClassCodes.contains(aircraftClassCode)
          case _ => false
        }
      }
    })
  })

  property("An aircraft can never be scheduled before its “target time”") = forAll(genAgenda)(la => {
    ScheduleMS01.assignRunway(la).fold({
      case DomainError.RepeatedAircraftId(_) => true
      case DomainError.RepeatedRunwayId(_) => true
      case DomainError.NoRunwaysAvailable(_) => true
      case DomainError.MaximumTimeWindowExceeded(_) => true
      case _ => false
    }, scheduleXML => {
      scheduleXML.forall { schedule =>
        val aircraftId = schedule.aircraft.id
        val aircraftOpt = la.aircrafts.find(_.id == aircraftId)

        aircraftOpt match {
          case Some(aircraft) =>
            val aircraftTargetTime = aircraft.target
            val scheduleTargetTime = schedule.actualTarget
            scheduleTargetTime >= aircraftTargetTime.NNItoInt
          case None => false
        }
      }
    })
  })

  property("In case of emergency, aircraft should land in the feasibility window") = forAll(genAgenda)(la => {
    ScheduleMS01.assignRunway(la).fold({
      case DomainError.RepeatedAircraftId(_) => true
      case DomainError.RepeatedRunwayId(_) => true
      case DomainError.NoRunwaysAvailable(_) => true
      case DomainError.MaximumTimeWindowExceeded(_) => true
      case _ => false
    }, scheduleXML => {
      scheduleXML.forall { schedule =>
        val aircraftId = schedule.aircraft.id
        val aircraftOpt = la.aircrafts.find(_.id == aircraftId)

        aircraftOpt match {
          case Some(aircraft) =>
            aircraft.emergency match {
              case Some(em) =>
                val actualTarget = schedule.actualTarget
                val emergencyRange = (aircraft.target.NNItoInt, aircraft.target.NNItoInt + em.PItoInt)
                actualTarget >= emergencyRange._1 && actualTarget <= emergencyRange._2
              case None => false
            }
          case None => false
        }
      }
    })
  })


  property("Ensure the necessary separation between runway operations on the same runway") = forAll(genAgenda)(la => {
    ScheduleMS01.assignRunway(la).fold({
      case DomainError.RepeatedAircraftId(_) => true
      case DomainError.RepeatedRunwayId(_) => true
      case DomainError.NoRunwaysAvailable(_) => true
      case DomainError.MaximumTimeWindowExceeded(_) => true
      case _ => false
    }, scheduleXML => {
      val runwaySchedules = scheduleXML.groupBy(_.runway)
      runwaySchedules.forall { case (_, schedules) =>
        val sortedSchedules = schedules.sortBy(_.actualTarget)

        if (sortedSchedules.sizeIs <= 1) {
          true
        } else {
          val adjacentSchedules = sortedSchedules.zip(sortedSchedules.drop(1))
          adjacentSchedules.forall { case (schedule1, schedule2) =>
            val aircraftId1 = schedule1.aircraft.id
            val aircraftOpt1 = la.aircrafts.find(_.id == aircraftId1)
            val aircraftId2 = schedule2.aircraft.id
            val aircraftOpt2 = la.aircrafts.find(_.id == aircraftId2)

            (aircraftOpt1, aircraftOpt2) match {
              case (Some(aircraft1), Some(aircraft2)) =>
                val separation = aircraftSeparation(aircraft1.aircraftClassParam, aircraft2.aircraftClassParam)
                val timeDifference = schedule2.actualTarget - schedule1.actualTarget
                timeDifference >= separation
              case _ => false
            }
          }
        }
      }
    })
  })

  property("The delay penalties are correctly calculated") = forAll(genAgenda)(la => {
    ScheduleMS01.assignRunway(la).fold({
      case DomainError.RepeatedAircraftId(_) => true
      case DomainError.RepeatedRunwayId(_) => true
      case DomainError.NoRunwaysAvailable(_) => true
      case DomainError.MaximumTimeWindowExceeded(_) => true
      case _ => false
    }, scheduleXML => {
      scheduleXML.forall { schedule =>
        val aircraftId = schedule.aircraft.id
        val aircraftOpt = la.aircrafts.find(_.id == aircraftId)

        aircraftOpt match {
          case Some(aircraft) =>
            val aircraftTargetTime = aircraft.target
            val scheduleTargetTime = schedule.actualTarget
            val delay = delayPenalty(aircraft.aircraftClassParam, scheduleTargetTime - aircraftTargetTime.NNItoInt)
            delay == schedule.cost
          case None => false
        }
      }
    })
  })


  property("The algorithm is deterministic") = forAll(genAgenda) { la =>
    ScheduleMS01.assignRunway(la).fold({
      case DomainError.RepeatedAircraftId(_) => true
      case DomainError.RepeatedRunwayId(_) => true
      case DomainError.NoRunwaysAvailable(_) => true
      case DomainError.MaximumTimeWindowExceeded(_) => true
      case _ => false
    }, v1Success => {

      ScheduleMS01.assignRunway(la).fold({
        case DomainError.RepeatedAircraftId(_) => true
        case DomainError.RepeatedRunwayId(_) => true
        case DomainError.NoRunwaysAvailable(_) => true
        case DomainError.MaximumTimeWindowExceeded(_) => true
        case _ => false
      }, v2Success => {
        v1Success.forall { v1Schedule =>
          v2Success.exists { v2Schedule =>
            v1Schedule.aircraft.id == v2Schedule.aircraft.id &&
              v1Schedule.runway == v2Schedule.runway &&
              v1Schedule.actualTarget == v2Schedule.actualTarget &&
              v1Schedule.cost == v2Schedule.cost
          }
        }
      })
    })
  }
