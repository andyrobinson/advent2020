package day23

import scala.annotation.tailrec

case class GameState(cups: Ring, currentCup: Long) {
  def fromOne(count: Long): String = cups.remove(1, count)._2.mkString("")
  def productOfNextTwo: Long = cups.next(1).toLong * cups.next(cups.next(1)).toLong
}

case class CrabGame(startingCups: List[Long]) {
  val maxCup = startingCups.max

  @tailrec
  final def play(currentCup:Long, iterations: Long, cups: Ring = Ring().splice(startingCups)): GameState = {
    if (iterations == 0) GameState(cups, currentCup)
    else {
      val newState = playRound(currentCup, cups)
      play(newState.currentCup, iterations-1, newState.cups)
    }
  }

  def playRound(currentCup: Long, cups: Ring = Ring().splice(startingCups)): GameState = {
    val (reducedCups, removed) = cups.remove(currentCup, 3)
    val nextCup = findDestinationCup(currentCup, removed)
    val newCups = reducedCups.splice(removed, Some(nextCup))
    val newCurrentCup = newCups.next(currentCup)
    GameState(newCups, newCurrentCup)
  }

  def findDestinationCup(currentCup: Long, removedCups: List[Long]): Long = {
    val next = (currentCup - 2 + maxCup) % maxCup + 1
    if (removedCups.contains(next)) findDestinationCup(next, removedCups.filter(_ != next)) else next
  }
}
