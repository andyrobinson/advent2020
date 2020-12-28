package day24

import scala.io.Source

object Main extends App {
  val lines = Source.fromResource("tiles.txt").getLines().toList
  val directions = HexTile.fromInput(lines)
  val blackTiles = HexTile.blackTiles(directions)
  val artwork = HexTile.flipRepeatedly(blackTiles,100)

  println("Answer1: " + blackTiles.size)
  println("Answer2: " + artwork.size)
}
