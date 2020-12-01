package day1

import scala.io.Source
import scala.collection.mutable.Set

object Main extends App {
  println("Calculating ...")
  val filename = "src/main/scala/day1/expenses.txt"

  var numbers: Set[Int] = Set.empty[Int]
  for (line <- Source.fromFile(filename).getLines) {
    numbers += line.toInt
  }

  numbers.foreach(i => {
    val answer = numbers.filter(j => i+j == 2020)
    if (answer.nonEmpty) println ("Answer1: " + answer.head * i)
  })

  numbers.foreach(i => {
    numbers.filter(j => i+j < 2020).foreach (j => {
      val answer = numbers.filter(k => i+j+k == 2020)
      if (answer.nonEmpty) println ("Answer2" + ": " + answer.head * i * j)
    })
  })
}
