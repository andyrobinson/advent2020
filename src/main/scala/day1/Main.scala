package day1

import scala.io.Source

object Main extends App {

  def search1(l: Seq[Int], target: Int) = l.find(_ == target)
  def search2(l: Seq[Int], target: Int) = doit(l, target, search1)

  def doit(num: Seq[Int], target : Int, fn: (Seq[Int], Int) => Option[Int]): Option[Int] =
      num.foldLeft(Option.empty[Int], num.tail)((acc, i) => {acc match {
        case (Some(result), _)  => (Some(result), Nil)
        case (None, l) if l.nonEmpty => (fn(l, target - i).map(_*i), l.tail)
        case (None, _) => (None, Nil)
      }})._1

  val filename = "expenses.txt"
  val numbers: Seq[Int] = Source.fromResource(filename).getLines.map(_.toInt).toList

  println("Answer1: " + doit(numbers, 2020, search1).get)
  println("Answer2: " + doit(numbers, 2020, search2).get)

}
