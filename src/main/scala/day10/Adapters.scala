package day10

case class PathCount(pathHead: Int, numberOfPathsToHere: Long)

object Adapters {
  def countPaths(adapters: List[Int]): Long = {
    val sortedAdapters = adapters.sorted
    val finalElement = sortedAdapters.last + 3
    (sortedAdapters :+ finalElement).foldLeft(List(PathCount(0,1))) {(pathcounts, next) =>
      val routes = routesWithin3(pathcounts, next)
      if (routes.size == 0) throw new RuntimeException("no complete paths!")
      else routes :+ PathCount(next, sumOfPathCounts(routes))
    }
  }.head.numberOfPathsToHere

  def sumOfPathCounts(routes: List[PathCount]): Long = {
    routes.foldLeft(0L)((sum, pc) => sum + pc.numberOfPathsToHere)
  }

  def routesWithin3(lowerValues: List[PathCount], newValue: Int): List[PathCount] =
     lowerValues.filter(_.pathHead >= newValue - 3)

  def findDifferences(adapters: List[Int]):(Int, Int) = {
    val result = adapters.sorted.foldLeft((0,1,0)) { case ((ones, threes, previous), adapter) =>
      (adapter - previous) match {
        case 1 => (ones + 1, threes, adapter)
        case 3 => (ones, threes + 1, adapter)
        case _ => throw new RuntimeException("unexpected difference")
      }
    }
    (result._1, result._2)
  }

}
