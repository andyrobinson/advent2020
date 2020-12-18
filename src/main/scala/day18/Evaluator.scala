package day18

sealed trait TokenTree
case class EmptyTree() extends TokenTree
case class Leaf(number: Long) extends TokenTree
case class Node(op: Char, left: TokenTree, right: TokenTree) extends TokenTree

object Evaluator {

  // reading the operators left to right translates to closed association right to left
  // it does look like the parentheses are backwards, but it works
  // it's a state machine with two states - probably better modelled explicitly, but hindsight is a wonderful thing
  // either we're parsing an operand or we're parsing an operator (given a previous
  // value) and looking for the value of the second operand so that we can calculate the value
  def evalRightToLeft(expression: List[Token]): Long = expr(expression.reverse)._1

  private def expr(expression: List[Token]): (Long, List[Token]) = expression match {
    case Number(x)::tl =>  opAndSecond(x,tl)
    case Paren(')')::tl => (opAndSecond _).tupled(expr(tl))
    case _ => throw new RuntimeException("Unexpected operand " + expression.headOption.getOrElse("None"))
  }

  // we thread the list of outstanding tokens through the calls because when we close a bracketed expression
  // (confusingly with an open bracket) and return the result, then we need to carry on and parse the rest
  // of the expression
  private def opAndSecond(acc: Long, opExpression: List[Token]): (Long, List[Token]) = opExpression match {
    case Nil => (acc, Nil)
    case Op('+')::tl => {val (value, l) = expr(tl); (acc + value, l)}
    case Op('*')::tl => {val (value, l) = expr(tl); (acc * value, l)}
    case  Paren ('(') :: tl => (acc, tl)
    case hd::_ => throw new RuntimeException("Unexpected operator " + hd)
  }

  // this actually works forwards or backwards.  Tried to refactor the two sets of functions into one
  // but too painful with the mutual recursion
  def evalPlusFirst(expression: List[Token]):Long = exprPlusFirst(expression.reverse)._1

  private def exprPlusFirst(expression: List[Token]): (Long, List[Token]) = expression match {
    case Number(x)::tl =>  opAndSecondPlusFirst(x,tl)
    case Paren(')')::tl => (opAndSecondPlusFirst _).tupled(exprPlusFirst(tl))
    case _ => throw new RuntimeException("Unexpected operand " + expression)
  }

  // the only extra code is that we need to pull out the value associated with the plus operation
  // and do the addition first before continuing with the recursion
  private def opAndSecondPlusFirst(acc: Long, opExpression: List[Token]): (Long, List[Token]) = opExpression match {
    case Nil => (acc, Nil)
    case Op('+')::Number(b)::tl => opAndSecondPlusFirst(acc + b, tl)
    case Op('+')::Paren(')')::tl => {val (value, l) = exprPlusFirst(tl); opAndSecondPlusFirst(acc + value, l)}
    case Op('+')::tl => {val (value, l) = exprPlusFirst(tl); (acc + value, l)}
    case Op('*')::tl => {val (value, l) = exprPlusFirst(tl); (acc * value, l)}
    case  Paren ('(') :: tl => (acc, tl)
    case hd::_ => throw new RuntimeException("Unexpected operator " + hd)
  }
}
