package day24

import scala.annotation.tailrec

object HexTile {

  @tailrec
  final def flipRepeatedly(blackTiles: Set[(Int,Int)], times: Int):Set[(Int,Int)] = {
    if (times == 0) blackTiles
    else flipRepeatedly(flip(blackTiles),times-1)
  }

  def neighbours(tile: (Int, Int)): Set[(Int,Int)] =
    vectors.values.toSet[(Int,Int)].map{ case (x,y) => (x + tile._1,y + tile._2)}

  def flip(blackTiles: Set[(Int, Int)]):Set[(Int,Int)] = {
    surviving(blackTiles) union whiteTilesToFlip(blackTiles)
  }

  private def whiteTilesToFlip(blackTiles: Set[(Int, Int)]) = {
    val tilesToConsider = blackTiles.flatMap(neighbours) diff blackTiles
    tilesToConsider.foldLeft(Set.empty[(Int, Int)]) { (acc, tile) =>
      if (neighbourCount(tile, blackTiles) == 2) acc + tile else acc
    }
  }

  def surviving(blackTiles: Set[(Int, Int)]) = {
    blackTiles.foldLeft(Set.empty[(Int, Int)]) { (acc, tile) =>
      neighbourCount(tile, blackTiles) match {
        case x if (x == 0 | x > 2) => acc
        case _ => acc + tile
      }
    }
  }

  private def neighbourCount(tile: (Int,Int), allTiles: Set[(Int,Int)]) : Int = {
    (neighbours(tile) intersect allTiles).size
  }

  def blackTiles(directions: List[List[String]]):Set[(Int,Int)] = {
    val tiles = directions.map(tilePath =>
      tilePath.map(vectors(_))
      .reduceLeft{ (coords, next) => (coords._1 + next._1,coords._2 + next._2)}
    )
    tiles.foldLeft(Set.empty[(Int,Int)]){(acc,tile) => if (acc.contains(tile)) acc - tile else acc + tile}
  }

  val vectors: Map[String, (Int,Int)] = Map(
    "e" -> (1,0),
    "w" -> (-1,0),
    "ne" -> (0,1),
    "nw" -> (-1,1),
    "se" -> (1,-1),
    "sw" -> (0,-1)
  )

  def readline(line: String): List[String] =
    if (line.isEmpty) Nil
    else line.head.toString match {
      case dir @ ("e" | "w") => dir :: readline(line.tail)
      case "n" | "s" => line.take(2) :: readline(line.drop(2))
      case _ => throw new RuntimeException("cannot parse input line: " + line)
    }

  def fromInput(input: List[String]): List[List[String]] =
    input.map(readline)
}
