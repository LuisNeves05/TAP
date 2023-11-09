package pj.domain.model.property

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import pj.domain.{DomainError, Result}
import pj.domain.model.{Agenda, Aircraft, Runway}
import pj.domain.schedule.ScheduleMS01
import pj.domain.types.{AircraftClass, Heavy, Identifier, Landing, Large, NonNegativeInteger, PositiveInteger, Small, TakeOff, Weight}
import pj.utils.Utils.aircraftClass
import pj.utils.Utils.{aircraftClass, aircraftClassNumber, aircraftSeparation, delayPenalty}

object PropertyGenBase:

  //OBJECTS
  def identifierGen: Gen[Identifier] = for {
    charList <- Gen.listOfN(3, Gen.alphaNumChar)
    identifier <- Identifier.from(charList.mkString).fold(_ => Gen.fail, Gen.const)
  } yield identifier

  def targetGen: Gen[NonNegativeInteger] = for {
    number <- Gen.choose(0, 100)
    result <- NonNegativeInteger.apply(number) match
      case Right(value) => Gen.const(value)
      case Left(_) => Gen.fail
  } yield result

  def emergencyGen: Gen[Option[PositiveInteger]] = for {
    intValue <- Gen.posNum[Int]
    positiveInteger <- PositiveInteger.apply(intValue) match
      case Right(positiveInt) => Gen.const(Some(positiveInt))
      case Left(_) => Gen.fail[Option[PositiveInteger]]
  } yield positiveInteger

  def maxDelayTimeGen: Gen[PositiveInteger] = for {
    number <- Gen.choose(20, 100)
    result <- PositiveInteger.apply(number) match
      case Right(value) => Gen.const(value)
      case Left(_) => Gen.fail
  } yield result

  def genAircraftClass: Gen[AircraftClass] =
    val classNumGen = Gen.oneOf("1", "2", "3", "4", "5", "6")

    classNumGen.flatMap { classNum =>
      classNum match
        case "1" => Gen.const(Landing(Small))
        case "2" => Gen.const(Landing(Large))
        case "3" => Gen.const(Landing(Heavy))
        case "4" => Gen.const(TakeOff(Small))
        case "5" => Gen.const(TakeOff(Large))
        case "6" => Gen.const(TakeOff(Heavy))
        case _ => Gen.fail
    }

  // LIST OF AIRCRAFTS GENERATION
  val aircraftClassListGen: Gen[List[AircraftClass]] =
    for {
      aircraftClassListGen <- Gen.containerOfN[Set, AircraftClass](6, genAircraftClass).map(_.toList)
    } yield aircraftClassListGen.distinct

  //RUNWAY
  val genRunway: Gen[Runway] =
    for {
      id <- identifierGen
      classes <- aircraftClassListGen
    } yield Runway(id, classes)

  //AIRCRAFT
  val genAircraft: Gen[Aircraft] =
    for
      id <- identifierGen
      target <- targetGen
      aircraftClassList <- aircraftClassListGen
      aircraftClass <- Gen.oneOf(aircraftClassList)
      emergency <- emergencyGen
    yield Aircraft(id, aircraftClass, target, emergency)


  // LIST OF RUNWAYS GENERATION
  val runwaysGen: Gen[List[Runway]] =
    for {
      genRunways <- Gen.listOfN(2, genRunway)
    } yield genRunways

  // LIST OF AIRCRAFTS GENERATION
  val aircraftsGen: Gen[List[Aircraft]] =
    for {
      genAircrafts <- Gen.listOfN(2, genAircraft)
    } yield genAircrafts

  //AGENDA
  val genAgenda: Gen[Agenda] =
    for
      aircrafts <- aircraftsGen
      runways <- runwaysGen
      maximumDelayTime <- maxDelayTimeGen
    yield Agenda(aircrafts, runways, maximumDelayTime)  
    
