package day17

import scala.collection.immutable.HashSet

object NDimensionalConway {

  var dimensionCache: Map[Int, List[List[Int]]] = Map.empty

  def cycle(grid: Grid): Grid = {
    val cellsToKeep = grid.filter{ cell =>
      val count = neighbourCount(cell,grid)
      count <=3 && count > 1
    }
    val cellsToConsider = grid.flatMap(neighbours(_))
    val cellsToGrow = cellsToConsider.filter(neighbourCount(_,grid) == 3)
    cellsToKeep union cellsToGrow
  }

  type Grid = Set[List[Int]]

  def neighbours(cell: List[Int]): Grid = {
    val dimensions = cell.size
    allDirections(dimensions).map{list => list.zip(cell).map{case (a,b) => a+b}}.toSet
  }

  private def neighbourCount(cell: List[Int], grid: Grid): Int = {
    (neighbours(cell) intersect(grid)).size
  }

  private def calculateAllDirections(dimensions: Int): List[List[Int]] = {
    val values = List.fill(dimensions)(-1) ++ List.fill(dimensions-1)(0) ++ List.fill(dimensions)(1)
    val allCells = values.combinations(dimensions).toList.map(_.permutations.toList)
    val flattenedCells = allCells.foldLeft(List.empty[List[Int]])((acc, value) => acc ++ value)
    dimensionCache = dimensionCache + (dimensions -> flattenedCells)
    flattenedCells
  }

  def allDirections(dimensions: Int):List[List[Int]] = {
    dimensionCache.getOrElse(dimensions,calculateAllDirections(dimensions))
  }


  def iterate(cubes: Grid, iterations: Int, iterationFn: Grid => Grid): Grid = {
    if (iterations == 0) cubes
    else iterate(iterationFn(cubes), iterations-1, iterationFn)
  }

  def gridFromLines(lines: List[String], dimensions: Int): Grid = {
    lines.zipWithIndex.foldLeft(HashSet.empty[List[Int]]) { case (acc, (line, y)) =>
      acc ++ line.toCharArray.zipWithIndex.flatMap { case (ch, x) => {
        ch match {
          case '#' => Some(List(x, y) ++ List.fill(dimensions-2)(0))
          case _ => None
        }
      }}
    }
  }
}
