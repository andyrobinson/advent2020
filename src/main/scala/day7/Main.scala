package day7

import scala.io.Source

object Main extends App {

  val rulesList = Source.fromResource("luggagerules.txt").getLines().toList
  val luggageRules = LuggageRules(rulesList)

  println("Answer1: " + luggageRules.bagsContaining("shiny gold").size)
  println("Answer2: " + luggageRules.bagsInside("shiny gold"))

}
