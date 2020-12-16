package day16

case class Range(lower: Int, upper: Int) {
  def inRange(testValue: Int): Boolean = testValue <= upper && testValue >= lower
}

case class Rule(name:String, validRanges: List[Range]) {
  def matches(value: Int): Boolean = validRanges.exists(_.inRange(value))
}

object Rule {
  val ruleRegEx = """^(.+):\s([0-9]+)-([0-9]+)\sor\s([0-9]+)-([0-9]+)""".r
  def fromLine(line:String): Rule = line match {
    case ruleRegEx(name,l1,u1,l2,u2) => Rule(name, List(Range(l1.toInt,u1.toInt),Range(l2.toInt,u2.toInt)))
    case _ => throw new RuntimeException("cannot parse rule line")
  }
}

case class Rules(rules: List[Rule]) {
  def validTickets(tickets: List[Ticket]) =
    tickets.filter(ticket => invalidTicketFields(ticket).isEmpty)

  def invalidTicketFields(ticket: Ticket):List[Int] =
    ticket.fields.filter(field => rules.forall(rule => !rule.matches(field)))

  def scanningErrorRate(tickets: List[Ticket]): Int =
    tickets.foldLeft(0){(acc,ticket) => acc + invalidTicketFields(ticket).sum}
}

object Rules {
  def fromLines(input: List[String]): Rules = {
    val ruleLines = input.takeWhile(!_.isEmpty())
    Rules(ruleLines.map(Rule.fromLine))
  }
}
