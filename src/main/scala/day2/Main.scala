package day2

import scala.io.Source

object Main extends App {

  val filename = "passwords.txt"

  val passwords = Source.fromResource(filename).getLines.map(Password.parse).toList

  println("Answer1: " + passwords.count(_.countWithinRange))
  println("Answer2: " + passwords.count(_.xorPresent))

}
