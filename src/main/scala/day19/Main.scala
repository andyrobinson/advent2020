package day19

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("matches.txt").getLines().toList
  val rules = Rules.fromInput(lines)
  val patterns = Rules.patternsFromInput(lines)
  val matching = Rules.matching(patterns,rules)
  val newRules = rules + (8 -> Rule(8,Set(SubRuleSeq(List(42)),SubRuleSeq(List(42,8))))) +
    (11 -> Rule(11,Set(SubRuleSeq(List(42,31)),SubRuleSeq(List(42,11,31)))))  // we can't parse this, but we can process it
  val matching2 = Rules.matching(patterns,newRules)

  println("Answer1: " + matching.size)
  println("Answer2: " + matching2.size)
}
