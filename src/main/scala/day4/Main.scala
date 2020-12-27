package day4

import utils.FileReader

import scala.io.Source

object Main extends App {

  val passportLines = FileReader.readBlankLineDelimited("passports.txt")

  val passportsWithRequiredFields = passportLines.foldLeft(0){(acc, line) =>
    if (!Passport.fromPairs(line).missingFields()) acc + 1 else acc
  }

  val validPassports = passportLines.foldLeft(0){(acc, line) =>
    if (Passport.fromPairs(line).isValid) acc + 1 else acc
  }

  println("Answer 1: " + passportsWithRequiredFields)
  println("Answer 2: " + validPassports)


}
