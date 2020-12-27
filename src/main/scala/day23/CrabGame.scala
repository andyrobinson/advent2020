package day23

import scala.annotation.tailrec

case class GameState(cups: Ring, currentCup: Int) {
  def fromOne(count: Int): String = cups.remove(1, count)._2.mkString("")
  def productOfNextTwo: Long = cups.next(1).toLong * cups.next(cups.next(1)).toLong
}

case class CrabGame(startingCups: List[Int]) {
  val maxCup = startingCups.max

  @tailrec
  final def play(currentCup:Int, iterations: Int, cups: Ring = Ring().splice(startingCups)): GameState = {
    if (iterations == 0) GameState(cups, currentCup)
    else {
      val newState = playRound(currentCup, cups)
      play(newState.currentCup, iterations-1, newState.cups)
    }
  }

  def playRound(currentCup: Int, cups: Ring = Ring().splice(startingCups)): GameState = {
    val (reducedCups, removed) = cups.remove(currentCup, 3)
    val nextCup = findDestinationCup(currentCup, removed)
    val newCups = reducedCups.splice(removed, Some(nextCup))
    val newCurrentCup = newCups.next(currentCup)
    GameState(newCups, newCurrentCup)
  }

  def findDestinationCup(currentCup: Int, removedCups: List[Int]): Int = {
    val next = (currentCup - 2 + maxCup) % maxCup + 1
    if (removedCups.contains(next)) findDestinationCup(next, removedCups.filter(_ != next)) else next
  }
}
