package day19

sealed trait Condition {
  def unmatchedPaths(pattern: String, rules: Map[Int, Rule]) : Set[String]
}

// we process everything based on the set of (as yet) unmatched patterns
case class SimpleMatch(str: String) extends Condition {
  override def unmatchedPaths(pattern: String, rules: Map[Int, Rule]): Set[String] = {
    if (pattern.startsWith(str)) {
      Set(pattern.substring(str.length))  // we've matched part, just return the rest
    } else Set.empty
  }
}

case class SubRuleSeq(rules: List[Int]) extends Condition {
  override def unmatchedPaths(pattern: String, rulesMap: Map[Int, Rule]): Set[String] =
    isPartialMatch2(pattern, rules, rulesMap)

  private def isPartialMatch2(pattern: String, rulesToApply: List[Int], rulesMap: Map[Int, Rule]): Set[String] = rulesToApply match {
    case Nil => if (pattern.isEmpty) Set("") else Set(pattern)
    case hd::tl => {
      // get all the unmatched patterns for the first rule, then attempt to match them with the remaining rules in the list
      val stillToMatch = rulesMap.get(hd).get.remainingPatterns(pattern, rulesMap)
      stillToMatch.foldLeft(Set.empty[String])((acc, patt) => acc union isPartialMatch2(patt, tl, rulesMap) )
    }
  }
}

