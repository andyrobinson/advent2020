package day3

import scala.io.Source

object Main extends App {

  val filename = "slope.txt"
  val lineList = Source.fromResource(filename).getLines.toList
  val slope = Slope.fromText(lineList)
  val slopeSize = lineList.size

  def treeCount(down: Int, along: Int, ylimit: Int): Int = {
    (0 to (ylimit/down)).foldLeft(0){(acc, i) =>
        if (slope.isTreeAt(i*along,i*down)) acc+1 else acc
    }
  }

  println("Answer 1: " + treeCount(1,3, slopeSize))

  var product: Long =  List((1,1), (1,3), (1,5), (1,7), (2,1)).foldLeft(1: Long) {
    case (acc, (down, along)) => treeCount(down,along, slopeSize) * acc
  }

  println("Answer 2: " + product)
}
