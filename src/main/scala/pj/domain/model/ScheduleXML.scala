package pj.domain.model

import pj.domain.types.NonNegativeInteger
import pj.domain.Result
import scala.xml.Elem
import pj.domain.model.Aircraft
import pj.domain.model.Runway
import pj.domain.DomainError.*

final case class ScheduleXML(aircraft: Aircraft, runway: Runway, actualTarget: Int, cost: Int)


object ScheduleXML:

  def exportToXML(scheduleXML: List[ScheduleXML]): Elem =
    val sortedSchedule = scheduleXML.sortBy(_.actualTarget)

    val aircraftElems = sortedSchedule.map { s =>
        <aircraft id={s.aircraft.id.identifierToString} runway={s.runway.id.identifierToString} time={s.actualTarget.toString}/>
    }

    <schedule cost={scheduleXML.foldLeft(0)((total, schedule) => total + schedule.cost).toString} xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2023 ../../schedule.xsd " xmlns="http://www.dei.isep.ipp.pt/tap-2023" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      {aircraftElems}
    </schedule>
