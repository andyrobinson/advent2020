package day12

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DirectionsSpec extends AnyFunSpec with Matchers {
  it("should read the input into a list of moves") {
    val lines = List("E3","F33","R90","N2","F25","N4","E1","S2","L90")
    val moves = Move.fromLines(lines)
    moves shouldBe List(Move('E',3), Move('F',33), Move('R',90), Move('N',2), Move('F',25), Move('N',4), Move('E',1), Move('S',2), Move('L',90))
  }

  it("should move in the specified direction") {
    val ship = Ship(coordinates = (0,0))
    Ship.move(Move('N',3),ship) shouldBe ship.copy(coordinates = (0,3))
    Ship.move(Move('S',4),ship) shouldBe ship.copy(coordinates = (0,-4))
    Ship.move(Move('E',5),ship) shouldBe ship.copy(coordinates = (5,0))
    Ship.move(Move('W',6),ship) shouldBe ship.copy(coordinates = (-6,0))
  }

  it("should rotate in the specified direction") {
    val ship = Ship(direction = Direction.west)
    Ship.move(Move('L', 90),ship) shouldBe ship.copy(direction = Direction.south)
    Ship.move(Move('L', 270),ship) shouldBe ship.copy(direction = Direction.north)
    Ship.move(Move('R', 180),ship) shouldBe ship.copy(direction = Direction.east)
    Ship.move(Move('R', 270),ship) shouldBe ship.copy(direction = Direction.south)
  }

  it("should move forward in the current direction") {
    val ship = Ship((10,20),Direction.south)
    Ship.move(Move('F',88),ship) shouldBe ship.copy(coordinates = (10,-68))
    val ship2 = Ship((10,20),Direction.east)
    Ship.move(Move('F',230),ship2) shouldBe ship2.copy(coordinates = (240,20))
  }

  it("should calculate the manhattan distance") {
    Ship((-44,88)).manhattanDistance shouldBe 132
  }

  it("should follow a list of moves") {
    val lines = List("N10","E10","S10","W10","R90","F10")
    val moves = Move.fromLines(lines)
    val ship = Ship()
    Ship.follow(moves,ship,Ship.move) shouldBe Ship((0,-10), Direction.south)
  }
}
