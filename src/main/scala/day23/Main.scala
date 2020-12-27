package day23

object Main extends App {
  private val startingList = List(4, 6, 7, 5, 2, 8, 1, 9, 3)
  val game = CrabGame(startingList)
  val result = game.play(4,100).fromOne(game.maxCup-1)

  val game2 = CrabGame(startingList ++ (10 to 1000000).toList)
  val result2 = game2.play(4, 10000000)

  println("Answer1: " + result)
  println("Answer2: " + result2.productOfNextTwo)
}
