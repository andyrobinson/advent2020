package day11

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SeatsSpec extends AnyFunSpec with Matchers {
  it("should read the lines into a map") {
    val input = List("LL.","L.L","..L")
    Seats.fromLines(input) shouldBe Seats(Map((0,0)->false,(1,0)->false,(0,1)->false,(2,1)->false,(2,2)->false),3,3)
  }

  it("should read occupied seats as well") {
    val input = List("LL.","#.L",".#L")
    Seats.fromLines(input) shouldBe Seats(Map((0,0)->false,(1,0)->false,(0,1)->true,(2,1)->false,(1,2)->true,(2,2)->false),3,3)
  }

  it("should make all seats occupied if there are no occupied seats") {
    val seats =Seats.fromLines(List("LL.","L.L","..L"))
    Seats.neighbourOccupation(seats) shouldBe Seats(Map((0,0)->true,(1,0)->true,(0,1)->true,(2,1)->true,(2,2)->true),3,3)
  }

  it("should only occupy seats with no occupied neighbours") {
    val seats = Seats.fromLines(List("L..","..#","..L"))
    Seats.neighbourOccupation(seats) shouldBe Seats(Map((0,0)->true, (2,1)->true,(2,2)->false),3,3)
  }

  it("should vacate seats with four or more neighbours") {
    val seats = Seats.fromLines(List("###","###","###"))
    Seats.neighbourOccupation(seats) shouldBe Seats.fromLines(List("#L#","LLL","#L#"))
  }

  it("should count occupied seats") {
    Seats.countOccupied(Seats.fromLines(List("#L#","LLL","#L#"))) shouldBe 4
  }

  it("should iterate to a stable configuration") {
    val seats = Seats.fromLines(List("L.L",".L.","L.L"))
    Seats.iterateUntilStable(seats, Seats.neighbourOccupation) shouldBe Seats.fromLines(List("#.#",".L.","#.#"))
  }

  it("visibleOccupation should make all seats occupied if there are no occupied seats") {
    val seats =Seats.fromLines(List("LL.","L.L","..L"))
    Seats.visibleOccupation(seats) shouldBe Seats.fromLines(List("##.","#.#","..#"))
  }

  it("visibleOccupation should occupy seats where there are no visible occupied seats") {
    val seats =Seats.fromLines(List(
      "#..L",
      ".L.L",
      "..L.",
      "L..."))
    Seats.visibleOccupation(seats) shouldBe Seats.fromLines(List(
      "#..L",
      ".L.#",
      "..#.",
      "L..."))
  }

}
