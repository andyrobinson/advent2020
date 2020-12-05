package day5

import scala.io.Source

object Main extends App {

  val filename = "boardingpasses.txt"

  val passes = Source.fromResource(filename).getLines.toList

  val seatIds = passes.map(pass =>
    Integer.parseInt(pass.replaceAll("[L|F]", "0").replaceAll("[R|B]","1"),2)
  ).sorted

  val missingAndLast = seatIds.foldLeft((0:Int, None: Option[Int])){(acc,seat) =>
    acc match {
      case (_, Some(answer)) => (seat, Some(answer))
      case (previousSeat, None) => if (seat - previousSeat == 2) (seat, Some(seat-1)) else (seat, None)
    }
  }

  println("Answer1: " + missingAndLast._1)
  println("Answer2: " + missingAndLast._2.get)

}
