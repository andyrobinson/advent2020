package day3

case class Slope(trees:List[(Int,Int)], xsize: Int) {
  def isTreeAt(x: Int, y: Int):  Boolean = {
    val x1 = x % xsize
    trees.exists(_ == (x1,y))
  }
}

object Slope {
  def fromText(lineList: List[String]):Slope = {

    val xsize = lineList.head.length

    val trees = lineList.zipWithIndex.map {case (line,n) =>
      line.toCharArray.toList.zipWithIndex.map {
        case ('#',i) => Some(i,n)
        case _ => None
      }.flatten
    }.flatten

    Slope(trees, xsize)
  }
}
