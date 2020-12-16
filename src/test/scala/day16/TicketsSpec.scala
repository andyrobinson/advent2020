package day16

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TicketsSpec extends AnyFunSpec with Matchers {
  it("should read the nearby tickets") {
    val input = List("duration: 25-131 or 151-967","","your ticket:","34,45,67","","nearby tickets:","852,748,166","696,714,108")
    Tickets.nearbyFromInput(input) shouldBe List(Ticket(List(852,748,166)),Ticket(List(696,714,108)))
  }

  it("should use the tickets to determine the fields") {
    val rules = Rules(List(Rule("field1",List(Range(10,20),Range(40,50))), Rule("field2",List(Range(5,20),Range(45,70))), Rule("field3",List(Range(1,10),Range(55,70)))))
    val tickets = List(Ticket(List(9,11,5)), Ticket(List(69,44,11)), Ticket(List(2,42,62)))
    val fieldMap = Tickets.findNameToIndex(tickets,rules)
    fieldMap shouldBe List(FieldIndex(0,"field3"), FieldIndex(1,"field1"), FieldIndex(2,"field2"))
  }

  it("should resolve differences if more than rule applies to a field") {
    val rules = Rules(List(Rule("fieldtwomatches",List(Range(10,20),Range(40,50))), Rule("field1",List(Range(5,20),Range(45,70))), Rule("field2",List(Range(1,10),Range(55,70)))))
    val tickets = List(Ticket(List(9,11,15)), Ticket(List(69,46,11)), Ticket(List(2,19,41)))
    val fieldMap = Tickets.findNameToIndex(tickets,rules)
    fieldMap shouldBe List(FieldIndex(0,"field2"), FieldIndex(2,"fieldtwomatches"), FieldIndex(1,"field1"))
  }

  it("should calculat the product of the fields beginning with departure") {
    val fieldMap = List(FieldIndex(1,"departure field1"), FieldIndex(0, "departure field2"), FieldIndex(2,"other field"))
    val ticket = Ticket(List(3,4,5))
    ticket.departureFieldProduct(fieldMap) shouldBe 12
  }


}
