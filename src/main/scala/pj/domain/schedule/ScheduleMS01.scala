package pj.domain.schedule

import scala.xml.Elem
import pj.domain.*
import pj.domain.model.ScheduleXML.*
import pj.domain.model.{Agenda, Aircraft, Runway, ScheduleXML}
import pj.io.FileIO.load
import pj.domain.DomainError.{NoRunwaysAvailable, RepeatedAircraftId, RepeatedRunwayId}
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}
import pj.utils.Utils.{aircraftClass, aircraftClassNumber, aircraftSeparation, delayPenalty}

import scala.annotation.tailrec
import scala.collection.immutable.Nil.:::

object ScheduleMS01 extends Schedule:

  // TODO: Create the code to implement a functional domain model for schedule creation
  //       Use the xml.XML code to handle the xml elements
  //       Refer to https://github.com/scala/scala-xml/wiki/XML-Processing for xml creatiom
  def create(xml: Elem): Result[Elem] =
    Agenda.fromXML(xml) match
      case Left(a) => Left(a)
      case Right(b) => assignRunway(b) match
        case Left(a) => Left(a)
        case Right(b) => Right(exportToXML(b))

  def assignRunway(agenda: Agenda): Result[List[ScheduleXML]] =

    // A tail-recursive function to schedule aircrafts on runways
    @tailrec
    def loop(remainingAircrafts: List[Aircraft], assigned: List[ScheduleXML]): Result[List[ScheduleXML]] =
    // Check if there are any remaining aircrafts to schedule
      remainingAircrafts match
        case Nil => Right(assigned) // Return the assigned list if there are no remaining aircrafts
        case aircraft :: aircrafts =>
          // Find runways that can accommodate the aircraft's class
          agenda.runways.filter(_.classes.contains(aircraft.aircraftClassParam)) match
            case Nil => Left(DomainError.NoRunwaysAvailable(aircraftClassNumber(aircraft.aircraftClassParam).getOrElse(""))) // Return an error if no runways are available for the aircraft's class
            case runway :: runways =>
              assigned match
                case Nil =>
                  // If no aircraft has been scheduled, assign the aircraft to the first available runway and continue scheduling the remaining aircrafts
                  loop(aircrafts, ScheduleXML(aircraft, runway, aircraft.target.NNItoInt, 0) :: Nil)
                case _ =>
                  // If aircrafts have already been scheduled, check the runways that are already assigned to aircrafts to avoid assigning multiple aircrafts to the same runway
                  val runwaysAvailable = runway :: runways
                  val map = runwaysAvailable.map(runway => (runway, assigned.filter(_.runway == runway))).toMap.withDefaultValue(List.empty[ScheduleXML])
                  val er = map.collectFirst { case (runway, t) if t.isEmpty => runway }
                  er match
                    case Some(runway) =>
                      // If a runway is empty, assign the aircraft to that runway and continue scheduling the remaining aircrafts
                      val update = assigned ::: (ScheduleXML(aircraft, runway, aircraft.target.NNItoInt, 0) :: Nil)
                      loop(aircrafts, update)
                    case None =>
                      // If no runway is empty, find the runway that will cause the least delay to the aircraft and assign the aircraft to that runway
                      findBestRunway(map, aircraft) match
                        case Right(bestRunway, lowestTime) =>
                          val delay = lowestTime - aircraft.target.NNItoInt
                          delay match
                            case d if d < agenda.maximumDelayTime.PItoInt =>
                              val cost = if (lowestTime > aircraft.target.NNItoInt) delayPenalty(aircraft.aircraftClassParam, d) else 0
                              val updatedTime = if (lowestTime < aircraft.target.NNItoInt) aircraft.target.NNItoInt else lowestTime
                              aircraft.emergency match
                                case Some(value) if (delay > value.PItoInt) =>
                                  findFitForEmergency(assigned, aircraft) match
                                    case Right(emerrunway, validSchedules, schedules, targetEmergency) =>
                                      val aircraftList: List[Aircraft] = schedules.map(_.aircraft)
                                      val cost = if (targetEmergency > aircraft.target.NNItoInt) delayPenalty(aircraft.aircraftClassParam, d) else 0
                                      if (runwaysAvailable.sizeIs > validSchedules.size && targetEmergency != aircraft.target.NNItoInt)
                                        val emergencySchedule = runwaysAvailable.filterNot(runway => validSchedules.exists(_.runway == runway))
                                        val emptyRunway = emergencySchedule.headOption
                                        emptyRunway match
                                          case Some(firstEmergencyRunway) =>
                                            loop(aircraftList ::: aircrafts, validSchedules ::: (ScheduleXML(aircraft, firstEmergencyRunway, aircraft.target.NNItoInt, 0) :: Nil))
                                          case None =>
                                            Left(DomainError.NoRunwaysAvailable(aircraftClassNumber(aircraft.aircraftClassParam).getOrElse("")))
                                      else
                                        loop(aircraftList ::: aircrafts, validSchedules ::: (ScheduleXML(aircraft, emerrunway, targetEmergency, cost) :: Nil))
                                    case Left(_) =>
                                      val aircraftList: List[Aircraft] = assigned.map(_.aircraft).filter(_.emergency.isEmpty)
                                      val emergencySchedule = assigned.filter(_.aircraft.emergency.isEmpty)
                                      val firstEmergencyRunwayOpt = emergencySchedule.headOption.map(_.runway)
                                      val emergencyScheduleT = assigned.filter(_.aircraft.emergency.isDefined)
                                      firstEmergencyRunwayOpt match
                                        case Some(firstEmergencyRunway) =>
                                          loop(aircraftList ::: aircrafts, emergencyScheduleT ::: (ScheduleXML(aircraft, firstEmergencyRunway, aircraft.target.NNItoInt, 0) :: Nil))
                                        case None =>
                                          Left(DomainError.NoRunwaysAvailable(aircraftClassNumber(aircraft.aircraftClassParam).getOrElse("")))
                                case None =>
                                  loop(aircrafts, assigned ::: (ScheduleXML(aircraft, bestRunway, updatedTime, cost) :: Nil))
                                case Some(_) =>
                                  loop(aircrafts, assigned ::: (ScheduleXML(aircraft, bestRunway, updatedTime, cost) :: Nil))
                            case _ =>
                              Left(DomainError.MaximumTimeWindowExceeded(aircraft.id.identifierToString + "," + lowestTime))
                        case Left(_) => Left(DomainError.NoRunwaysAvailable(aircraftClassNumber(aircraft.aircraftClassParam).getOrElse("")))
    loop(agenda.aircrafts.sortBy(_.target), Nil)

  private def findBestRunway(value: Map[Runway, List[ScheduleXML]], aircraft: Aircraft): Result[(Runway, Int)] =
    val runwayTime = value.toSeq.collect {
      case (runway, t) if t.nonEmpty =>
        val time = t.reverse.foldLeft(Int.MinValue)((maxTime, prevAircraft) =>
          math.max(maxTime, prevAircraft.actualTarget + aircraftSeparation(prevAircraft.aircraft.aircraftClassParam, aircraft.aircraftClassParam))
        )
        (runway, time)
    }
    runwayTime match
      case head +: tail =>
        val (runway, time) = tail.foldLeft(head) {
          case (minTime, currTime) => if (currTime._2 < minTime._2) currTime else minTime
        }
        Right((runway, time))
      case Nil =>
        Left(DomainError.NoRunwaysAvailable(aircraftClassNumber(aircraft.aircraftClassParam).getOrElse("")))

  private def findFitForEmergency(value: List[ScheduleXML], aircraft: Aircraft): Result[(Runway, List[ScheduleXML], List[ScheduleXML], Int)] =
    @tailrec
    def loop(remaining: List[ScheduleXML], acc: List[ScheduleXML], runwayInterdict: List[String]): Option[(Runway, List[ScheduleXML], List[ScheduleXML], Int)] =
      remaining match
        case Nil => None
        case head :: tail =>
          head.runway.classes match
            case x if x.contains(aircraft.aircraftClassParam) =>
              runwayInterdict match
                case x if !x.contains(head.runway.id.identifierToString) =>
                  val separationTime = aircraftSeparation(head.aircraft.aircraftClassParam, aircraft.aircraftClassParam)
                  aircraft.emergency match
                    case Some(value) =>
                      val proposedEndTime = value + aircraft.target.NNItoInt
                      if (separationTime + head.actualTarget > proposedEndTime)
                        loop(tail, head :: acc, runwayInterdict)
                      else
                        val currentTime = separationTime + head.actualTarget
                        val emergencyInterval = (aircraft.target.NNItoInt, value + aircraft.target.NNItoInt)
                        val landingTime = if (currentTime >= emergencyInterval._1 && currentTime < emergencyInterval._2) // check if x is inside y interval
                          currentTime + 1 // return x if it's inside y
                        else
                          emergencyInterval._1 // return the lower bound if x is less than y
                        Some((head.runway, remaining, acc, landingTime))
                    case None =>
                      head.aircraft.emergency match
                        case Some(_) =>
                          loop(tail, acc, head.runway.id.identifierToString :: runwayInterdict)
                        case None =>
                          loop(tail, head :: acc, runwayInterdict)
                case _ =>
                  loop(tail, acc, runwayInterdict)
            case _ =>
              loop(tail, head :: acc, runwayInterdict)
    loop(value.reverse, Nil, Nil) match
      case Some((runway, schedules, nvschedules, target)) =>
        Right((runway, schedules.reverse, nvschedules, target))
      case None => Left(DomainError.NoRunwaysAvailable(aircraftClassNumber(aircraft.aircraftClassParam).getOrElse("")))

