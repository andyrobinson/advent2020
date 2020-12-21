package day20

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("imagetiles.txt").getLines().toList
  val tiles = Tiles.fromInput(lines)
  val result = Tiles.findCornerProduct(tiles, 12)

  println("Answer1: " + result)
  println("Answer2: " + 0)
}
