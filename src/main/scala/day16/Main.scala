package day16

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("tickets.txt").getLines().toList
  val rules = Rules.fromLines(lines)
  val nearbyTickets = Tickets.nearbyFromInput(lines)
  val myTicket: Ticket = Tickets.myTicket(lines)
  val fieldMappings = Tickets.findNameToIndex(nearbyTickets, rules)

  println("Answer1: " + rules.scanningErrorRate(nearbyTickets))
  println("Answer2: " + myTicket.departureFieldProduct(fieldMappings))
}


