package day6

import utils.FileReader

object Main extends App {

  private val delimiter = ":"
  val landingcards = FileReader.readBlankLineDelimited("landingcards.txt",delimiter)
  val sumOfAnswers = landingcards.foldLeft(0)((acc,landingcard) => acc + landingcard.replaceAll(delimiter,"").toSet.size)

  val sumOfCommonAnswers = landingcards.foldLeft(0) {(acc, landingcard) =>
    val answerSets = landingcard.split(delimiter).map(_.toSet)
    acc + answerSets.reduceLeft((commonAnswers, answer) => commonAnswers.intersect(answer)).size
  }

  println("Answer1: " + sumOfAnswers)
  println("Answer2: " + sumOfCommonAnswers)

}
