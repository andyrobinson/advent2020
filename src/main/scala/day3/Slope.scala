package day3

case class Slope(treeCoordinates:List[(Int,Int)], width: Int) {
  def isTreeAt(x: Int, y: Int):  Boolean = {
    treeCoordinates.contains((x % width, y)) // mod simulates repeating pattern horizontally
  }
}

object Slope {
  def fromText(lineList: List[String]):Slope = {

    val width = lineList.head.length

    val treeCoordinates = lineList.zipWithIndex.flatMap { case (line, n) =>
      line.toCharArray.toList.zipWithIndex.flatMap {
        case ('#', i) => Some(i, n)
        case _ => None
      }
    }

    Slope(treeCoordinates, width)
  }
}
