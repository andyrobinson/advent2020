package utils

import scala.io.Source

object FileReader {

  def readBlankLineDelimited(sourceFile: String, partDelimiter: String = " "): Seq[String] = {
    val unterminatedBrokenLines = Source.fromResource(sourceFile).getLines().toList
    val brokenlines =  if (unterminatedBrokenLines.last.isEmpty) unterminatedBrokenLines else unterminatedBrokenLines :+ ""

    brokenlines.foldLeft((List.empty[String], "": String)) {case ((list, linesofar), line) =>
      if (line.isEmpty) (list :+ linesofar.trim, "") else {
        if (linesofar.isEmpty) (list, line) else (list, linesofar + partDelimiter + line)
      }
    }._1
  }

}
