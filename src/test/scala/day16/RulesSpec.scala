package day16

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class RulesSpec extends AnyFunSpec with Matchers {
  it("should read a list of rules from the input") {
    val input = List("arrival track: 43-703 or 719-965", "class: 29-822 or 828-961","duration: 25-131 or 151-967","","yourticket","34,45,67")
    Rules.fromLines(input) shouldBe Rules(List(
      Rule("arrival track",List(Range(43,703),Range(719,965))),
      Rule("class",List(Range(29,822),Range(828,961))),
      Rule("duration",List(Range(25,131),Range(151,967)))
    ))
  }

  it("should list the invalid fields in the ticket using the rules") {
    val rules = Rules(List(Rule("1",List(Range(10,20),Range(40,50))), Rule("2",List(Range(5,10),Range(30,40)))))
    val ticket = Ticket(List(4,10,15,20,21,25,30,33,50,52))
    rules.invalidTicketFields(ticket) shouldBe List(4,21,25,52)
  }

  it("should sum all the invalid fields for a list of tickets to give the scanning error rate") {
    val rules = Rules(List(Rule("1",List(Range(10,20),Range(40,50))), Rule("2",List(Range(5,10),Range(30,40)))))
    val tickets = List(Ticket(List(10,20,30,40)), Ticket(List(10,15,20,25,30,33,50, 51)), Ticket(List(2,3,15,17,29,102)))
    rules.scanningErrorRate(tickets) shouldBe (25 + 51 + 2 + 3 + 29 + 102)
  }

  it("should return the valid tickets") {
    val rules = Rules(List(Rule("1",List(Range(10,20),Range(40,50))), Rule("2",List(Range(5,10),Range(30,40)))))
    val tickets = List(Ticket(List(10,20,30,40)), Ticket(List(10,15,20,25,30,33,50, 51)), Ticket(List(2,3,15,17,29,102)))
    rules.validTickets(tickets) shouldBe List(Ticket(List(10,20,30,40)))
  }


}
