package day22

import scala.io.Source

object Main extends App {
  val lines = Source.fromResource("cards.txt").getLines().toList
  val decks = Cards.fromInput(lines)
  val result = Cards.combat(decks)
  val finalScore = Math.max(Cards.score(result._1),Cards.score(result._2))
  val rcResult = Cards.recursiveCombat(decks)

  println("Answer1: " + finalScore)
  println("Answer2: " + rcResult._3)
}
