package day11

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("seats.txt").getLines().toList
  val seats = Seats.fromLines(lines)

  val stableOccupiedUsingNeighbours = Seats.countOccupied(Seats.iterateUntilStable(seats,Seats.neighbourOccupation))

  println("Answer1: " + stableOccupiedUsingNeighbours)
  println("Answer2: " + 0)
}
