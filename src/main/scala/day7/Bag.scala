package day7

case class Bag(description: String, innerBags: List[(Int, String)]) {
  def contains(name: String): Boolean = innerBags.exists(_._2 == name)
  def innerBagNames: List[String] = innerBags.map(_._2)
}

object Bag {
  def fromRule(line: String): Bag = {
    val cleanedLine = line.replaceAll("contain|bags\\.","").replaceAll("bags,|bag,|bags|bag", ":")
    val ruleparts = cleanedLine.split(":").toList.map(_.trim)
    Bag(ruleparts.head, ruleparts.tail.flatMap(makeEntry))
  }

  private def makeEntry(entry: String): Option[(Int, String)] = {
    val ruleRegEx = """^(\d+)\s([a-z]+\s[a-z]+)$""".r

    entry match {
      case ruleRegEx(count, description) => Some((count.toInt, description))
      case _ => None
    }
  }
}
