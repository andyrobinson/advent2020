package day10

import scala.io.Source

object Main extends App {

  val adapters = Source.fromResource("adapters.txt").getLines().toList.map(_.toInt)
  val (ones, threes) = Adapters.findDifferences(adapters)

  println("Answer1: " + ones * threes)
  println("Answer2: " + Adapters.countPaths(adapters))
}
