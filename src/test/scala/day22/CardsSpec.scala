package day22

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CardsSpec extends AnyFunSpec with Matchers {
  it("should read two decks of cards") {
    val input = List("Player 1:","1","2","3","","Player 2:","4","5","6" )
    val decks = Cards.fromInput(input)
    decks shouldBe (List(1,2,3),List(4,5,6))
  }

  it("should play a single round") {
    Cards.playRound((List(1,2),List(3,4))) shouldBe (List(2),List(4,3,1))
  }
  
  it("should play until winning") {
    Cards.combat ((List(1,2),List(3,4))) shouldBe (List(),List(3,1,4,2))
  }

  it("should calculated winning score") {
    Cards.score(List(3,1,4,2)) shouldBe (4*3 + 3*1 + 4*2 + 2)
  }

  it("should not loop infinitely") {
    Cards.recursiveCombat((List(43,19),List(2,29,14))) shouldBe ("Player 1", (List(43, 19),List(2, 29, 14)), 105)
  }

  it("should play recursive combat") {
    Cards.recursiveCombat((List(9, 2, 6, 3, 1), List(5, 8, 4, 7, 10))) shouldBe ("Player 2", ((List(),List(7, 5, 6, 2, 4, 1, 10, 8, 9, 3))), 291)
  }
}
