package day19

case class Rule(id: Int, alternativeConditions: Set[Condition]) {
  def isValid(pattern: String, rules: Map[Int, Rule]): Boolean = alternativeConditions.exists {condition =>
    condition.unmatchedPaths(pattern, rules).contains("")  // if the final set of unmatched patterns contains an empty pattern, we have matched
  }

  // gather all the unmatched patterns from all the subrules
  def remainingPatterns(pattern: String, rules: Map[Int, Rule]): Set[String] =
    alternativeConditions.foldLeft(Set.empty[String]) {(acc,condition) =>
      acc union condition.unmatchedPaths(pattern, rules)
    }

}

object Rule {

  val simpleMatchRegex = """^([0-9]+):\s\"([a|b])\"$""".r
  val singleRuleRegEx = """^([0-9]+):\s([0-9]+)$""".r
  val twoRulesRegEx = """^([0-9]+):\s([0-9]+)\s([0-9]+)$""".r
  val alternatesRegEx = """^([0-9]+):\s([0-9]+)\s([0-9]*)\s*\|\s([0-9]+)\s*([0-9]*)$""".r

  def fromLine(line: String): Rule = line match {
    case simpleMatchRegex(id,ch) => Rule(id.toInt, Set(SimpleMatch(ch)))
    case singleRuleRegEx(id,rule) => Rule(id.toInt, Set(SubRuleSeq(List(rule.toInt))))
    case twoRulesRegEx(id,rule1,rule2) => Rule(id.toInt, Set(SubRuleSeq(List(rule1.toInt,rule2.toInt))))
    case alternatesRegEx(id,rule1,rule2,rule3,rule4) => Rule(id.toInt, Set(SubRuleSeq(oneOrTwo(rule1,rule2)),SubRuleSeq(oneOrTwo(rule3,rule4))))
    case _ => throw new RuntimeException("cannot parse line " + line)
  }

  private def oneOrTwo(strInt1: String, strMaybeInt2: String): List[Int] = {
    List(strInt1.toIntOption, strMaybeInt2.toIntOption).flatten
  }

}

object Rules {
  def matching(patterns: List[String], rules: Map[Int, Rule]):Set[String] =
    patterns.toSet.filter(pattern =>rules(0).isValid(pattern, rules))

  def patternsFromInput(value: List[String]):List[String] = value.dropWhile(_.nonEmpty).filter(_.nonEmpty)

  def fromInput(lines: List[String]): Map[Int,Rule] = lines.takeWhile(_ != "").map{line =>
     val rule = Rule.fromLine(line)
     rule.id -> rule
   }.toMap

}