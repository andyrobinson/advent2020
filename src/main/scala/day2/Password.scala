package day2

case class Password(point1: Int, point2: Int, limitedCharacter: Char, password: String) {
  def xorPresent: Boolean = {
      (point1 <= password.length && password.charAt(point1 - 1) == limitedCharacter) ^
      (point2 <= password.length && password.charAt(point2 - 1) == limitedCharacter)
  }

  def countWithinRange: Boolean = {
    val occurrences = password.count(_ == limitedCharacter)
    occurrences >= point1 && occurrences <= point2
  }
}

object Password {
  val passwordRegEx = """^(\d+)-(\d+)\s([a-z]):\s([a-zA-z]+)$""".r

  def parse(value: String): Password = {
    value match {
      case passwordRegEx(lower,upper,ch,pwd) => Password(lower.toInt, upper.toInt, ch.charAt(0), pwd)
      case _ => throw new RuntimeException("cannot match")
    }
  }
}