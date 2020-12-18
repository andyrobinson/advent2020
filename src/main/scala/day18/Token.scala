package day18

sealed trait Token
case class Op(ch: Char) extends Token
case class Number(n: Long) extends Token
case class Paren(ch: Char) extends Token

object Token {
  def fromInput(input: List[String]):List[List[Token]] = {
    input.map(tokenise)
  }

  val digits = "0123456789".toSet

  def tokenise(line: String): List[Token] = line match {
    case ""  =>  List.empty[Token]
    case l =>  l.head match {
      case ' ' => tokenise(l.tail)
      case digit if digits.contains(digit) => Number(l.takeWhile(digits.contains).toLong) :: tokenise(l.dropWhile(digits.contains))
      case '+' | '*' => Op(l.head)::tokenise(l.tail)
      case '(' | ')' => Paren(l.head)::tokenise(l.tail)
      case _ => throw new RuntimeException("Unexpected token")
    }
  }
}