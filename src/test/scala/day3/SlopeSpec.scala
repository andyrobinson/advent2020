package day3

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SlopeSpec extends AnyFunSpec with Matchers {
  it("parses input lines into a list of co-ordinates") {
    val rawInput = List("..#..##", ".#.....", "###....")
    val slope = Slope.fromText(rawInput)
    slope.treeCoordinates shouldBe List((2,0),(5,0),(6,0),(1,1),(0,2),(1,2),(2,2))
  }

  it("indicates if there is a tree") {
    val slope = Slope(List((2,1),(4,1),(3,2)),5)
  }

  it("indicates if there is no tree") {
    val slope = Slope(List((2,1),(4,1),(3,2)),5)
    slope.isTreeAt(1,1) shouldBe false
    slope.isTreeAt(2,2) shouldBe false
  }

  it("repeats the pattern of trees across the slope") {
    val slope = Slope(List((2,1),(4,1),(3,2)),5)
    slope.isTreeAt(4,1) shouldBe true
    slope.isTreeAt(9,1) shouldBe true
    slope.isTreeAt(14,1) shouldBe true
    slope.isTreeAt(19,1) shouldBe true
  }

  it("reports no trees if we go off the bottom"){
    val slope = Slope(List((2,1),(4,1),(3,2)),5)
    slope.isTreeAt(4,100) shouldBe false
  }
}



