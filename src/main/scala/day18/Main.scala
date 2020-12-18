package day18

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("expressions.txt").getLines().toList
  val tokenList = Token.fromInput(lines)
  val sum = tokenList.map(Evaluator.evalRightToLeft).sum
  val sum2 = tokenList.map(Evaluator.evalPlusFirst).sum

  println("Answer1: " + sum)
  println("Answer2: " + sum2)
}
