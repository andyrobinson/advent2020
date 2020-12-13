name := "advent2020"

version := "0.1"

scalaVersion := "2.13.4"

lazy val day1 = project in file("day1")
lazy val day2 = project in file("day2")
lazy val day3 = project in file("day3")
lazy val day4 = project in file("day4")
lazy val day5 = project in file("day5")
lazy val day6 = project in file("day6")
lazy val day7 = project in file("day7")
lazy val day8 = project in file("day8")
lazy val day9 = project in file("day9")
lazy val day10 = project in file("day10")
lazy val day11 = project in file("day11")
lazy val day12 = project in file("day12")
lazy val day13 = project in file("day13")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"