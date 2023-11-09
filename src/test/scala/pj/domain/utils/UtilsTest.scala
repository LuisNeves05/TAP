package pj.domain.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pj.domain.DomainError
import pj.domain.types.{Heavy, Landing, Large, Small, TakeOff}
import pj.utils.Utils.{aircraftClass, aircraftClassNumber, aircraftSeparation, delayPenalty}

import scala.language.adhocExtensions

class UtilsTest extends AnyFlatSpec with Matchers {

    "aircraftSeparation" should "return the correct separation distance for two aircraft" in {
      aircraftSeparation(Landing(Small), Landing(Small)) shouldEqual 82
      aircraftSeparation(Landing(Small), Landing(Large)) shouldEqual 69
      aircraftSeparation(Landing(Small), Landing(Heavy)) shouldEqual 60
      aircraftSeparation(Landing(Small), TakeOff(Small)) shouldEqual 75
      aircraftSeparation(Landing(Large), Landing(Small)) shouldEqual 131
      aircraftSeparation(Landing(Large), Landing(Large)) shouldEqual 69
      aircraftSeparation(Landing(Large), Landing(Heavy)) shouldEqual 60
      aircraftSeparation(Landing(Large), TakeOff(Small)) shouldEqual 75
      aircraftSeparation(Landing(Heavy), Landing(Small)) shouldEqual 196
      aircraftSeparation(Landing(Heavy), Landing(Large)) shouldEqual 157
      aircraftSeparation(Landing(Heavy), Landing(Heavy)) shouldEqual 96
      aircraftSeparation(Landing(Heavy), TakeOff(Small)) shouldEqual 75
      aircraftSeparation(TakeOff(Heavy), TakeOff(Large)) shouldEqual 120
      aircraftSeparation(TakeOff(Heavy), TakeOff(Small)) shouldEqual 120
      aircraftSeparation(TakeOff(Heavy), TakeOff(Heavy)) shouldEqual 90
      aircraftSeparation(TakeOff(Large), Landing(Small)) shouldEqual 60
    }

  "aircraftClass" should "return the correct AircraftClass for a given string" in {
    aircraftClass("1") shouldEqual Right(Landing(Small))
    aircraftClass("2") shouldEqual Right(Landing(Large))
    aircraftClass("3") shouldEqual Right(Landing(Heavy))
    aircraftClass("4") shouldEqual Right(TakeOff(Small))
    aircraftClass("5") shouldEqual Right(TakeOff(Large))
    aircraftClass("6") shouldEqual Right(TakeOff(Heavy))
    aircraftClass("") shouldEqual Left(DomainError.IllegalClassNumber("Identifier cannot be empty"))
    aircraftClass("7") shouldEqual Left(DomainError.IllegalClassNumber("Identifier cannot be empty"))
  }

    "aircraftClassNumber" should "return the correct string representation for a given AircraftClass" in {
      aircraftClassNumber(Landing(Small)) shouldEqual Right("1")
      aircraftClassNumber(Landing(Large)) shouldEqual Right("2")
      aircraftClassNumber(Landing(Heavy)) shouldEqual Right("3")
      aircraftClassNumber(TakeOff(Small)) shouldEqual Right("4")
      aircraftClassNumber(TakeOff(Large)) shouldEqual Right("5")
      aircraftClassNumber(TakeOff(Heavy)) shouldEqual Right("6")
    }

  "delayPenalty" should "return the correct penalty for a given operation and delay" in {
    delayPenalty(Landing(Small), 0) shouldEqual 0
    delayPenalty(Landing(Small), 10) shouldEqual 20
    delayPenalty(TakeOff(Heavy), 5) shouldEqual 5
    delayPenalty(Landing(Heavy), -1) shouldEqual 0
  }
}