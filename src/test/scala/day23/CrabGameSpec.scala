package day23

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CrabGameSpec extends AnyFunSpec with Matchers {

  it ("should find the next cup, wrapping around zero") {
    val game = CrabGame(List(1,2,3,4,5,6))
    game.findDestinationCup(4, Nil) shouldBe 3
    game.findDestinationCup(1, Nil) shouldBe 6
    game.findDestinationCup(6, Nil) shouldBe 5
  }

  it ("should skip numbers in the list when finding the next cup") {
    val game = CrabGame(List(1, 2, 3, 4, 5, 6))
    game.findDestinationCup(3, List(6,1,2)) shouldBe 5
  }

  it("should play a single round") {
    val game  = CrabGame(List(1,2,3,4,5,6,7))
    val gameState = game.playRound(2)
    gameState.currentCup shouldBe 6
    gameState.cups.links shouldBe Map(1->3,3 -> 4,4->5,5->2,2->6,6->7,7->1)
  }

  it("should play move 4 round from the question") {
    val game  = CrabGame(List(7,2,5,8,9,1,3,4,6))
    val gameState = game.playRound(8)
    gameState.currentCup shouldBe 4
    gameState.cups.links shouldBe Map(3->2, 2->5, 5->8, 8->4, 4->6, 6->7, 7->9, 9->1, 1-> 3)
  }

  it("should play multiple moves") {
    val game  = CrabGame(List(3,8,9,1,2,5,4,6,7))
    val gameState = game.play(3,10)
    gameState.currentCup shouldBe 8
    gameState.cups.links shouldBe Map(5->8,8->3,3->7,7->4,4->1,1->9,9->2,2->6,6->5)
    gameState.fromOne(game.maxCup-1) shouldBe "92658374"

    val gs2 = game.play(3,100)
    gs2.fromOne(game.maxCup-1) shouldBe "67384529"
  }

}

