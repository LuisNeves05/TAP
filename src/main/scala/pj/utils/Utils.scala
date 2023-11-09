package pj.utils

import pj.domain.DomainError.IllegalClassNumber
import pj.domain.Result
import pj.domain.types.{AircraftClass, Heavy, Landing, Large, Small, TakeOff}

object Utils:

  def aircraftSeparation(leading: AircraftClass, trailing: AircraftClass): Int =
    (leading, trailing) match
      case (Landing(Small), Landing(Small)) => 82
      case (Landing(Small), Landing(Large)) => 69
      case (Landing(Small), Landing(Heavy)) => 60
      case (Landing(Small), _) => 75

      case (Landing(Large), Landing(Small)) => 131
      case (Landing(Large), Landing(Large)) => 69
      case (Landing(Large), Landing(Heavy)) => 60
      case (Landing(Large), _) => 75

      case (Landing(Heavy), Landing(Small)) => 196
      case (Landing(Heavy), Landing(Large)) => 157
      case (Landing(Heavy), Landing(Heavy)) => 96
      case (Landing(Heavy), _) => 75

      case (TakeOff(Heavy), TakeOff(Large)) => 120
      case (TakeOff(Heavy), TakeOff(Small)) => 120
      case (TakeOff(Heavy), TakeOff(Heavy)) => 90
      case (_, _) => 60

  def aircraftClass(classNum: String): Result[AircraftClass]  = classNum match
    case "1" => Right(Landing(Small))
    case "2" => Right(Landing(Large))
    case "3" => Right(Landing(Heavy))
    case "4" => Right(TakeOff(Small))
    case "5" => Right(TakeOff(Large))
    case "6" => Right(TakeOff(Heavy))
    case _ => Left(IllegalClassNumber("Identifier cannot be empty"))

  def aircraftClassNumber(classNum: AircraftClass): Result[String] = classNum match
    case Landing(Small) => Right("1")
    case Landing(Large) => Right("2")
    case Landing(Heavy) => Right("3")
    case TakeOff(Small) => Right("4")
    case TakeOff(Large) => Right("5")
    case TakeOff(Heavy) => Right("6")
    case _ => Left(IllegalClassNumber("Invalid class"))

  def delayPenalty(operation: AircraftClass, delay: Int): Int =
    delay match
      case penalty if penalty > 0 =>
        operation match
          case Landing(Small) | Landing(Large) | Landing(Heavy) => penalty * 2
          case TakeOff(Small) | TakeOff(Large) | TakeOff(Heavy) => penalty
      case _ => 0


