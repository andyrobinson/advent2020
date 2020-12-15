package day15

import collection.mutable.LongMap // improves efficiency about threefold

object ElfGame {
  def nthNumberSpoken(startingNumbers: List[Long], finalTurn: Long):Long = {
    val startingMap = LongMap[Long]()
    startingNumbers.zipWithIndex.foreach{case (number, turn) => startingMap.addOne(number, turn + 1)}
    nthNumber(startingMap, startingNumbers.last, finalTurn, startingNumbers.size)
  }

  private def nthNumber(previouslySpoken: LongMap[Long], lastSpoken: Long, finalTurn: Long, lastTurn: Long):Long = {
    if (lastTurn == finalTurn) lastSpoken
    else {
      val numberToSpeak: Long = previouslySpoken.get(lastSpoken).map(turn => lastTurn - turn).getOrElse(0)
      nthNumber(previouslySpoken.addOne(lastSpoken,lastTurn), numberToSpeak, finalTurn, lastTurn + 1)
    }
  }
}
