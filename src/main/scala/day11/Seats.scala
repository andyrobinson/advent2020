package day11

case class Seats(seating: Map[(Int, Int), Boolean], width: Int, height: Int)

object Seats {

  def iterateUntilStable(seats: Seats, iterationFn: Seats => Seats): Seats = {
    val newSeats = iterationFn(seats)
    if (newSeats != seats) iterateUntilStable(newSeats, iterationFn) else seats
  }

  def countOccupied(seats: Seats): Int = seats.seating.values.count(_ == true)

  def neighbourOccupation(seats: Seats): Seats =
    seats.copy(seating = seats.seating.map(seat => neighbourCount(seat._1, seats) match {
      case 0 => seat.copy(_2 = true)
      case x if x >= 4 => seat.copy(_2 = false)
      case _ => seat
    }))

  private def neighbourCount(seat: (Int,Int), seats: Seats): Int = {
    List((-1, -1),(0, -1),(1, -1),(-1, 0),(1, 0),(-1, 1),(0, 1),(1, 1)).foldLeft(0) {case (acc, (xOffset, yOffset)) =>
      if (seats.seating.getOrElse((seat._1 + xOffset, seat._2 + yOffset), false)) acc + 1 else acc
    }
  }

  def visibleOccupation(seats: Seats): Seats =
    seats.copy(seating = seats.seating.map(seat => visibleCount(seat._1, seats) match {
      case 0 => seat.copy(_2 = true)
      case x if x >= 5 => seat.copy(_2 = false)
      case _ => seat
    }))

  private def visibleCount(seat: (Int,Int), seats: Seats): Int = {
    List((-1, -1),(0, -1),(1, -1),(-1, 0),(1, 0),(-1, 1),(0, 1),(1, 1)).foldLeft(0) {case (acc, vector) =>
      if (visibleOccupiedSeat(seat, vector, seats)) acc + 1 else acc
    }
  }

  private def visibleOccupiedSeat(coordinates: (Int, Int), vector: (Int, Int), seats: Seats): Boolean = false


  def fromLines(lines: List[String]): Seats = {
    val height = lines.size
    val width = lines.head.length
    val seating = lines.zipWithIndex.foldLeft(Map.empty[(Int,Int),Boolean]) { case (acc, (line, y)) =>
      acc ++ line.toCharArray.zipWithIndex.flatMap { case (ch, x) => {
        ch match {
          case 'L' => Some((x, y) -> false)
          case '#' => Some((x, y) -> true)
          case _ => None
        }
      }}
    }
    Seats(seating, width, height)
  }
}
