package day12

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("navigation.txt").getLines().toList
  val moves = Move.fromLines(lines)
  val dist = Ship.follow(moves,Ship(),Ship.move).manhattanDistance
  val waypointDist = Ship.follow(moves,Ship(),Ship.moveWaypointorShip).manhattanDistance

  println("Answer1: " + dist)
  println("Answer2: " + waypointDist)
}
