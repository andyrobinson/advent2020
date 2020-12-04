name := "advent2020"

version := "0.1"

scalaVersion := "2.13.4"

lazy val day1 = project in file("day1")
lazy val day2 = project in file("day2")
lazy val day3 = project in file("day3")
lazy val day4 = project in file("day4")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"