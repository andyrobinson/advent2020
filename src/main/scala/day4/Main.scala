package day4

import scala.io.Source

object Main extends App {

  val filename = "passports.txt"

  val brokenlines = Source.fromResource(filename).getLines().toList :+ "" // to force last line

  val passportLines = brokenlines.foldLeft((List.empty[String], "": String)) {case ((list, linesofar), line) =>
     if (line.isEmpty) (list :+ linesofar.trim, "") else (list, linesofar + " " + line)
   }._1

  val validPassports = passportLines.foldLeft(0){(acc, line) =>
    if (Passport.fromPairs(line).isValid) acc + 1 else acc
  }

  println("Answer 1: " + validPassports)

}
