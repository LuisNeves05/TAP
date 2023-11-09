package pj.domain.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import pj.domain.DomainError
import pj.domain.model.Aircraft.parseAircraft
import pj.domain.types.{Identifier, NonNegativeInteger, PositiveInteger}
import pj.utils.Utils.{aircraftClass, aircraftClassNumber}

import scala.language.adhocExtensions

class AircraftClassTest extends AnyFlatSpec with Matchers {

  "AircraftClass" should "be created with valid values" in {
    val landing = Landing(Small)
    landing.weight shouldBe Small

    val takeoff = TakeOff(Heavy)
    takeoff.weight shouldBe Heavy
  }

  "Weight" should "be created with valid values" in {
    val small = Small
    small.toString shouldBe "Small"

    val large = Large
    large.toString shouldBe "Large"

    val heavy = Heavy
    heavy.toString shouldBe "Heavy"
  }
}