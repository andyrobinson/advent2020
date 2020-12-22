package day20

case class Tile(id:Int, sides:List[String], content:List[String] = List.empty[String]) {
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

    // the inside 8x8 square
    val contents = tileLines.tail.take(8).map(l => l.tail.take(8))
    Tile(id,List(top,right,bottom,left),contents)
  }

  def rotateLeft(tile:Tile, quadrants:Int): Tile = tile.copy(sides = listRotateLeft(tile.sides,quadrants), content = rotateContentLeft(tile.content, quadrants))

  def flipHoriz(tile: Tile):Tile = tile.copy(sides=List(tile.sides(Top),tile.sides(Left),tile.sides(Bottom),tile.sides(Right)).map(_.reverse),
    content = flipContentHoriz(tile.content))

  def permutations(tile: Tile): Set[Tile] = {
    (0 to 3).foldLeft(Set.empty[Tile]){(acc, i) =>
      val tl = rotateLeft(tile,i)
      acc union Set(tl, flipHoriz(tl))}
  }

  private def flipContentHoriz(value: List[String]): List[String] = {
    val size = value.headOption.fold(0)(_.size)
    (0 to size-1).foldLeft(List.empty[String]){(acc,i) =>
      acc :+ (size-1 to 0 by -1).foldLeft("")((str, j) => str + value(i)(j))
    }
  }

  private def rotateContentLeft(value: List[String], quadrants: Int): List[String] = {
    if (quadrants % 4 == 0) value
    else {
      val size = value.headOption.fold(0)(_.size)
      val leftOne = (size-1 to 0 by -1).foldLeft(List.empty[String]){(acc,i) =>
        acc :+ (0 to size-1).foldLeft("")((str, j) => str + value(j)(i))
      }
      rotateContentLeft(leftOne, quadrants -1)
    }
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

