package day16

case class FieldIndex(index: Int, fieldName: String)
case class FieldMatches(index: Int, fieldNames: List[String])

case class Ticket(fields: List[Int]) {
  def departureFieldProduct(fieldMap: List[FieldIndex]): Long =
    fieldMap
      .filter(_.fieldName.startsWith("departure"))
      .foldLeft(1L){ case (acc,FieldIndex(index,_)) => acc * fields(index).toLong }
}

object Tickets {
  def nearbyFromInput(lines: List[String]): List[Ticket] = {
    val nearbyTicketLines = lines.dropWhile(_ != "nearby tickets:").tail
    nearbyTicketLines.map(line => ticketFromLine(line))
  }

  def myTicket(lines: List[String]): Ticket  = {
    val myTicketLine = lines.dropWhile(_ != "your ticket:").tail.head
    ticketFromLine(myTicketLine)
  }

  def findNameToIndex(nearbyTickets: List[Ticket], rules: Rules): List[FieldIndex] = {
    val validTickets = rules.validTickets(nearbyTickets)
    val indexedMatches  = (0 to rules.rules.size-1).map { index =>
      val matchingRules = rules.rules.filter(rule => fieldsAtIndex(validTickets,index).forall(field => rule.matches(field)))
      if (matchingRules.isEmpty) throw new RuntimeException ("Cannot find matching rule for index " + index + " field values: " + fieldsAtIndex(validTickets,index))
      FieldMatches(index,matchingRules.map(_.name))
    }.toList

    resolveMultipleMatches(indexedMatches)
  }

  // we resolve multiple matches by choosing ones with a single match first (hence we sort first by number of matches) and then removing the field
  // from the outstanding list of matches.  By the time we get to ones with multiple matches, we should have removed all but one.
  // If we encounter anything other than a single match as we progress down the list then we fail
  private def resolveMultipleMatches(indexedMatches: List[FieldMatches]): List[FieldIndex] = {
    val sortedMatches = indexedMatches.sortBy(_.fieldNames.size)
    sortedMatches match {
      case Nil => List.empty[FieldIndex]
      case hd::tl if (hd.fieldNames.size == 1) => FieldIndex(hd.index, hd.fieldNames.head) :: resolveMultipleMatches(remove(hd.fieldNames.head,tl))
      case _ => throw new RuntimeException("ambiguous or missing rule match detected " + sortedMatches.head)
    }
  }

  private def remove(fieldname: String, indexedMatches: List[FieldMatches]): List[FieldMatches] =
        indexedMatches.map{fm => fm.copy(fieldNames = fm.fieldNames.filter(_ != fieldname))}

  private def fieldsAtIndex(tickets: List[Ticket], index: Int): List[Int] =
    tickets.map(_.fields(index))

  private def ticketFromLine(line: String) = {
    Ticket(line.split(",").toList.map(_.toInt))
  }

}
