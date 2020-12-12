package day12

case class Move(cmd: Char, amount: Int)

object Move {
  val moveRegEx = """^([N|S|E|W|L|R|F])([0-9]+)$""".r

  def fromLines(lines: List[String]): List[Move] = lines.map {
    case moveRegEx(cmd, amountStr) => Move(cmd.charAt(0), amountStr.toInt)
    case _ => throw new RuntimeException("cannot parse input")
  }

}