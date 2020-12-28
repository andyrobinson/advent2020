package day23

import scala.collection.mutable.LongMap // for performance

case class Ring() {
  var links: LongMap[Long] = LongMap()

  def remove(after: Long, count: Long, removedSoFar: List[Long] = Nil): (Ring, List[Long]) =
    if (count == 0) (this, removedSoFar.reverse)
    else {
      val toRemove = links(after)
      links.addOne(after, links(toRemove))
      links.subtractOne(toRemove)
      remove(after, count-1, toRemove :: removedSoFar)
    }

  def splice(values: List[Long], position: Option[Long] = None): Ring =
    values.foldLeft(this,position){ (acc, i) =>
      (acc._1.add(i,acc._2),Some(i))
    }._1

  def next(startingPoint: Long):Long = links(startingPoint)

  def add(value: Long, position: Option[Long] = None): Ring = {
    links.addOne(value, (if (position.isDefined) links(position.get) else value))
    links.addOne((if (position.isDefined) position.get else value),value)
    this
  }
}
