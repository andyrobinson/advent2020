package day9

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("ciphertext.txt").getLines().toList
  val numbers = lines.flatMap(x => if (x.isEmpty) None else Some(x.toLong))

  val preamble = numbers.slice(0,25)
  val rest = numbers.slice(25, numbers.size)
  val invalidNumber: Long = Cipher.findInvalid(preamble, rest)
  val range = Cipher.findRange(invalidNumber, numbers).sorted

  println("Answer1: " + invalidNumber)
  println("Answer2: " + (range.head + range.last))
}
