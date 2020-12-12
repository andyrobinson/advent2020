package day12

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class WaypointDirectionsSpec extends AnyFunSpec with Matchers {

  it("should move waypoint in the specified direction") {
    val ship = Ship(waypoint = (3,4))
    Ship.moveWaypointorShip(Move('N',3),ship) shouldBe ship.copy(waypoint = (3,7))
    Ship.moveWaypointorShip(Move('S',4),ship) shouldBe ship.copy(waypoint = (3,0))
    Ship.moveWaypointorShip(Move('E',5),ship) shouldBe ship.copy(waypoint = (8,4))
    Ship.moveWaypointorShip(Move('W',6),ship) shouldBe ship.copy(waypoint = (-3,4))
  }

  it("should rotate waypoint in the specified direction") {
    val ship = Ship(waypoint = (10,2))
    Ship.moveWaypointorShip(Move('L', 90),ship) shouldBe ship.copy(waypoint = (-2,10))
    Ship.moveWaypointorShip(Move('R', 270),ship) shouldBe ship.copy(waypoint = (-2,10))

    Ship.moveWaypointorShip(Move('L', 270),ship) shouldBe ship.copy(waypoint = (2,-10))
    Ship.moveWaypointorShip(Move('R', 90),ship) shouldBe ship.copy(waypoint = (2,-10))

    Ship.moveWaypointorShip(Move('R', 180),ship) shouldBe ship.copy(waypoint = (-10,-2))
    Ship.moveWaypointorShip(Move('L', 180),ship) shouldBe ship.copy(waypoint = (-10,-2))
  }

  it("should move forward in multiples of the waypoint") {
    val ship = Ship(coordinates=(3,5), waypoint = (2,4))
    Ship.moveWaypointorShip(Move('F',5),ship) shouldBe ship.copy(coordinates = (13,25))
    val ship2 = Ship(coordinates=(3,5), waypoint = (-2,4))
    Ship.moveWaypointorShip(Move('F',10),ship2) shouldBe ship2.copy(coordinates = (-17, 45))
  }


  it("should follow a list of moves") {
    val lines = List("N10","E10","F5","S10","W10","R90","F10")
    val moves = Move.fromLines(lines)
    val ship = Ship(waypoint = (2,2))
    Ship.follow(moves,ship,Ship.moveWaypointorShip) shouldBe Ship((80,40), Direction.east, (2,-2))
  }

}
