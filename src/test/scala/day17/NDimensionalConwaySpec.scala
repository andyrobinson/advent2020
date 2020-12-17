package day17

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class NDimensionalConwaySpec extends AnyFunSpec with Matchers {
  it("should generate all the neighbour co=ordinates") {
    NDimensionalConway.allDirections(3).size shouldBe 26 // three cubed less one for the actual cell
    NDimensionalConway.allDirections(4).size shouldBe 80 // three to the power of four less one
  }

  it("should read the 2D grid at z0") {
    val grid = NDimensionalConway.gridFromLines(List(".#.","##.","..#"),3)
    grid shouldBe Set(List(1,0,0),List(0,1,0),List(1,1,0),List(2,2,0))

    val grid4 = NDimensionalConway.gridFromLines(List(".#.","##.","..#"),4)
    grid4 shouldBe Set(List(1,0,0,0),List(0,1,0,0),List(1,1,0,0),List(2,2,0,0))
  }

  it("should calculate the neighbours of a cell") {
    val neighbours = NDimensionalConway.neighbours(List(4,5,6))
    //neighbours.size shouldBe 26
    neighbours shouldBe Set(
      List(3,4,5),List(3,5,5),List(3,6,5),List(4,4,5),List(4,5,5),List(4,6,5),List(5,4,5),List(5,5,5),List(5,6,5),
      List(3,4,6),List(3,5,6),List(3,6,6),List(4,4,6),        List(4,6,6),List(5,4,6),List(5,5,6),List(5,6,6),
      List(3,4,7),List(3,5,7),List(3,6,7),List(4,4,7),List(4,5,7),List(4,6,7),List(5,4,7),List(5,5,7),List(5,6,7))
  }

  it("should kill cells with less than two neighbours") {
    val grid1 = Set(List(1,0,0),List(0,1,0))
    NDimensionalConway.cycle(grid1) shouldBe Set.empty
  }

  it("should generate cells with three neighbours and cells with two or three neighbours") {
    val grid3inLine = Set(List(1,0,-1),List(1,0,0),List(1,0,1))
    NDimensionalConway.cycle(grid3inLine) shouldBe Set(List(0,-1,0), List(0,0,0), List(1,0,0), List(2,-1,0), List(0,1,0), List(1,-1,0), List(2,0,0), List(2,1,0), List(1,1,0))

    val grid2AllTouching = Set(List(1,0,0),List(0,1,0),List(1,1,0))
    NDimensionalConway.cycle(grid2AllTouching) shouldBe Set(List(1,1,0), List(0,0,-1), List(1,0,1), List(0,0,0), List(0,1,-1), List(1,0,-1), List(1,1,-1), List(0,1,1), List(0,1,0), List(1,1,1), List(1,0,0), List(0,0,1))
  }

}
