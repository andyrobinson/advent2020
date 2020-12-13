package day13

case class Buses(departTimestamp: Int, busTimesAndIndex: List[(Int,Int)]) {
  def earliestAvailable : (Int,Int) = {
    val busTimes = busTimesAndIndex.map(_._1)
    val busNumber = busTimes.reduceLeft((bestsofar:Int, next: Int) =>
      if (waitTime(next) < waitTime(bestsofar)) next else bestsofar)
    (busNumber, waitTime(busNumber))
  }

  private def waitTime(busTime: Int):Int = busTime - (departTimestamp % busTime)

  // we can optimise by doing a smaller part of the list and using this to increase the increment
  // note this only works because the bus numbers are all primes
  // implied by (... there will be exactly one such bus)
  // the point at which they work is the starting point for our bigger loop
  // and the increment is their product

  def earliestTimestamp(): Long = {
    val sortedBuses = busTimesAndIndex.sorted.reverse
    val firstFewBuses = sortedBuses.take(4) // the execution time is very sensitive to the number we choose here!
    val slowestBus = firstFewBuses.head

    // precalculation step
    val firstFewSolution = earliest(firstFewBuses, (slowestBus._1 - slowestBus._2), slowestBus._1)
    val busProduct = firstFewBuses.foldLeft(1)((acc: Int, value) => acc * value._1)

    // final calculation step
    earliest(sortedBuses, firstFewSolution, busProduct)
  }

  private def earliest(buses:List[(Int,Int)], start: Long, increment: Long): Long = {
    val timestamp = start + increment
    if (alldeparturesMatch(timestamp, buses)) timestamp
    else earliest(buses, timestamp, increment)
  }

  private def alldeparturesMatch(timestamp: Long, buses: List[(Int,Int)]) : Boolean =
    buses.forall{case (bus,index) => (timestamp + index) % bus == 0}

}

object Buses {
  def fromInput(input: List[String]): Buses = {
    val depart = input.head.toInt
    val validBuses = input.tail.head.split(",").zipWithIndex.flatMap {
      case (str,index) if str.toIntOption.isDefined => Some((str.toInt,index))
      case _ => None
    }.toList
    Buses (depart, validBuses)
  }
}