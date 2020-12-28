package day23

object Main extends App {
  private val startingList = List(4L, 6L, 7L, 5L, 2L, 8L, 1L, 9L, 3L)
  val game = CrabGame(startingList)
  val result = game.play(4,100).fromOne(game.maxCup-1)

  val game2 = CrabGame(startingList ++ (10L to 1000000L).toList)
  val result2 = game2.play(4, 10000000)

  println("Answer1: " + result)
  println("Answer2: " + result2.productOfNextTwo)
}
