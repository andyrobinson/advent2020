package day20

case class Tile(id:Int, sides:List[String]) {
  import Tile._
  def acceptsBottom(tile2: Tile):Boolean = this.sides(Bottom) == tile2.sides(Top).reverse
  def acceptsRight(tile2: Tile):Boolean = this.sides(Right) == tile2.sides(Left).reverse
}

object Tile {
  val Top = 0
  val Right = 1
  val Bottom = 2
  val Left = 3

  def fromLines(lines: List[String]): Tile = {
    val atFirstLine = lines.dropWhile(!_.startsWith("Tile"))
    val id = getTileId(atFirstLine.head)
    val tileLines = atFirstLine.tail.take(10)

    // note sides are always read left-to-right as though the side was at the top
    val top = tileLines.head
    val right = tileLines.map(_.last).mkString("")
    val bottom = tileLines.last.reverse
    val left = tileLines.map(_.head).reverse.mkString("")
    Tile(id,List(top,right,bottom,left))
  }

  def rotateLeft(tile:Tile, quadrants:Int): Tile = tile.copy(sides = listRotateLeft(tile.sides,quadrants))

  def flipHoriz(tile: Tile):Tile = tile.copy(sides=List(tile.sides(Top),tile.sides(Left),tile.sides(Bottom),tile.sides(Right)).map(_.reverse))

  def permutations(tile: Tile): Set[Tile] = {
    (0 to 3).foldLeft(Set.empty[Tile]){(acc, i) =>
      val tl = rotateLeft(tile,i)
      acc union Set(tl, flipHoriz(tl))}
  }

  private def listRotateLeft(seq: List[String], i: Int): List[String] = {
    val size = seq.size
    seq.drop(i % size) ++ seq.take(i % size)
  }

  private def getTileId(str:String):Int = {
    val idRegEx = """^Tile\s([0-9]+)\:$""".r
    str match {
      case idRegEx(id) => id.toInt
      case _ => throw new RuntimeException("id not found")
    }
  }
}

