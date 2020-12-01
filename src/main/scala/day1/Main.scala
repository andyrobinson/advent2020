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
    numbers.find(j => i+j == 2020).map { answer =>
      println ("Answer1: " + answer * i)
    }
  })

  numbers.foreach(i => {
    numbers.filter(j => i+j < 2020).foreach (j => {
      numbers.find(k => i+j+k == 2020).map { answer =>
        println ("Answer2" + ": " + answer * i * j)
      }
    })
  })
  
}
