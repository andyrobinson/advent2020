package day17

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("cubes.txt").getLines().toList
  val grid3 = NDimensionalConway.gridFromLines(lines,3)
  val grid4 = NDimensionalConway.gridFromLines(lines,4)
  val answer1 = NDimensionalConway.iterate(grid3,6,NDimensionalConway.cycle).size
  val answer2 = NDimensionalConway.iterate(grid4,6,NDimensionalConway.cycle).size

  println("Answer1: " + answer1)
  println("Answer2: " + answer2)
}
