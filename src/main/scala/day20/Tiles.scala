package day20

case class Tiles(tiles: List[Tile]) {
  def allPermutations(): List[Tile] = tiles.map(Tile.permutations).flatten // 144 * 8 tiles
}

object Tiles {
  def fromInput(lines:List[String]): Tiles =
    Tiles(getTiles(lines))

  private def getTiles(lines: List[String]): List[Tile] = {
    if (lines.dropWhile(_.isEmpty).isEmpty) Nil
    else Tile.fromLines(lines)::getTiles(lines.drop(12))
  }

  def findCornerProduct(tiles: Tiles, gridSize: Int):Long = {
    val grid = findGrid(tiles, gridSize)
    val gs = gridSize -1
    (grid(0,0).id.toLong * grid(0,gs).id.toLong * grid(gs, 0).id.toLong * grid(gs,gs).id.toLong)
  }

  def findGrid(tiles:Tiles,gridSize:Int): Map[(Int,Int),Tile] = {
    val (grid, found) = findGrid1(Map.empty[(Int,Int),Tile],tiles.allPermutations(),(0,0),gridSize)
    if (found) grid
    else throw new RuntimeException("No arrangement found")
  }

  private def findTilesThatFit(remainingTiles: List[Tile], gridSoFar: Map[(Int, Int), Tile], nextElement: (Int, Int)):List[Tile] = {
    nextElement match {
      case (0,0) => remainingTiles
      case (0,y) => remainingTiles.filter(gridSoFar(0,y-1).acceptsBottom(_))
      case(x,0) => remainingTiles.filter(gridSoFar(x-1,0).acceptsRight(_))
      case(x,y) => remainingTiles.filter(tl => gridSoFar(x,y-1).acceptsBottom(tl) && gridSoFar(x-1,y).acceptsRight(tl))
    }
  }

  private def calculateNext(coords: (Int, Int), gridSize: Int): (Int, Int) = {
    val x = (coords._1 + 1) % gridSize
    val y = if (x == 0) coords._2 + 1 else coords._2
    (x,y)
  }

  private def findGrid1(gridSoFar: Map[(Int,Int),Tile], remainingTiles:List[Tile], nextElement: (Int,Int), gridSize:Int): (Map[(Int,Int),Tile], Boolean) = {
    if (gridSoFar.size == gridSize * gridSize) (gridSoFar,true)
    else {
      val possibleTiles = findTilesThatFit(remainingTiles, gridSoFar, nextElement)
      possibleTiles.foldLeft((Map.empty[(Int,Int),Tile], false)){(acc, tile) =>
        if (acc._2) acc // shortcut for found
        else findGrid1(gridSoFar + (nextElement -> tile), remainingTiles.filterNot(_.id == tile.id), calculateNext(nextElement, gridSize),gridSize)
      }
    }
  }
}
