package day13

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BusesSpec extends AnyFunSpec with Matchers {
  it("should read the input") {
    val buses = Buses.fromInput(List("1234", "6,8,x,x,x,x,x,11,x,x,x,x,13"))
    buses shouldBe Buses(1234, List((6,0),(8,1),(11,7),(13,12)))
  }

  it("should calculate the earliest bus available") {
    val buses = Buses(29, List((7,1),(11,3),(13,7)))
    buses.earliestAvailable shouldBe (11,4)
  }

  it("should calculated the earliest timestamp where the buses depart according to their offsets") {
    Buses.fromInput(List("000", "67,7,59,61")).earliestTimestamp shouldBe 754018
    Buses.fromInput(List("000", "67,x,7,59,61")).earliestTimestamp shouldBe 779210
    Buses.fromInput(List("000", "67,7,x,59,61")).earliestTimestamp shouldBe 1261476
    Buses.fromInput(List("000", "1789,37,47,1889")).earliestTimestamp shouldBe 1202161486
  }
}




