package day13

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("buses.txt").getLines().toList
  private val buses = Buses.fromInput(lines)
  val (bus, waitTime) = buses.earliestAvailable
  val ts = buses.earliestTimestamp()

  println("Answer1: " + (bus * waitTime))
  println("Answer2: " + ts)
}


