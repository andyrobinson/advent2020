package day21

import scala.io.Source

object Main extends App {
  val lines = Source.fromResource("labels.txt").getLines().toList
  val labels: FoodLabels = FoodLabels.fromInput(lines)

  println("Answer1: " + labels.allergenFreeTotal())
  println("Answer2: " + labels.dangerousList())
}
