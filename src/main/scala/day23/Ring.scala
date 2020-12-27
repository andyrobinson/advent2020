package day23

import scala.collection.mutable.Map  // this helps somewhat with performance.  I wonder if LongMap will now help more.

case class Ring() {
  var links: Map[Int,Int] = Map()

  def remove(after: Int, count: Int, removedSoFar: List[Int] = Nil): (Ring, List[Int]) =
    if (count == 0) (this, removedSoFar.reverse)
    else {
      val toRemove = links(after)
      links.addOne(after, links(toRemove))
      links.subtractOne(toRemove)
      remove(after, count-1, toRemove :: removedSoFar)
    }

  def splice(values: List[Int], position: Option[Int] = None): Ring =
    values.foldLeft(this,position){ (acc, i) =>
      (acc._1.add(i,acc._2),Some(i))
    }._1

  def next(startingPoint: Int):Int = links(startingPoint)

  def add(value: Int, position: Option[Int] = None): Ring = {
    links.addOne(value, (if (position.isDefined) links(position.get) else value))
    links.addOne((if (position.isDefined) position.get else value),value)
    this
  }
}
