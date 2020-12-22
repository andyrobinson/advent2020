package day20

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("imagetiles.txt").getLines().toList
  val tiles = Tiles.fromInput(lines)
  val result = Tiles.findCornerProduct(tiles, 12)
  val grid = Tiles.findCompleteGrid(tiles, 12)
  val hashes = Tiles.hashes(grid)
  val monsters = Tile.permutations(grid).map(Tiles.countMonsters(_)).find(_ > 0).getOrElse(0)
  val finalResult = hashes - (monsters * Kraken.hashes)

  println("Answer1: " + result)
  println("Answer2: " + finalResult)
}
