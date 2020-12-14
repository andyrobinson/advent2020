package day14

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("maskmem.txt").getLines().toList
  val memMap = MemMap.fromLines(lines)
  val decodedMap = MemMap.AddressDecode(lines)

  println("Answer1: " + memMap.maskedSum)
  println("Answer2: " + decodedMap.plainSum)
}


