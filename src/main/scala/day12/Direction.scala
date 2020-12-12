package day12

case class Direction(name: Char, bearing: Int, moveVector: (Int, Int))

object Direction {

  val north = Direction('N', 0, (0,1))
  val east = Direction('E', 90, (1,0))
  val south = Direction('S', 180, (0, -1))
  val west = Direction('W', 270, (-1,0))

  val compassPoints = List(north, east, south, west)

}