package day18

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EvaluatorSpec extends AnyFunSpec with Matchers {
  it("should tokenise an input line") {
    val line = "1 + (200 * 3)"
    Token.tokenise(line) shouldBe List(Number(1),Op('+'),Paren('('),Number(200),Op('*'),Number(3),Paren(')'))
  }

  it("should read the input into a list of expressions") {
    val input = List("1 + (200 * 3)","4 * (100 + 22)")
    Token.fromInput(input) shouldBe List(
      List(Number(1),Op('+'),Paren('('),Number(200),Op('*'),Number(3),Paren(')')),
      List(Number(4),Op('*'),Paren('('),Number(100),Op('+'),Number(22),Paren(')')))
  }

  it("should evaulate a single number expression") {
    Evaluator.evalRightToLeft(Token.tokenise("44")) shouldBe 44
  }

  it("should evaulate a simple addition") {
    Evaluator.evalRightToLeft(Token.tokenise("10 + 5")) shouldBe 15
  }

  it("should evaulate a simple multiplication") {
    Evaluator.evalRightToLeft(Token.tokenise("11 * 12")) shouldBe 132
  }

  it("should evaluate an expression using brackets") {
    Evaluator.evalRightToLeft(Token.tokenise("11 * (5 + 13)")) shouldBe 198
  }

  it("should evaluate an expression without brackets left to right") {
    Evaluator.evalRightToLeft(Token.tokenise("11 * 5 + 13")) shouldBe 68
  }

  it("should evaluate a more complex expression using brackets") {
    Evaluator.evalRightToLeft(Token.tokenise("((1 + 2) * 3) + ((4 * 2) + 1)")) shouldBe 18
  }

  it("should do the examples in the question") {
    Evaluator.evalRightToLeft(Token.tokenise("2 * 3 + (4 * 5)")) shouldBe 26
    Evaluator.evalRightToLeft(Token.tokenise("5 + (8 * 3 + 9 + 3 * 4 * 3) ")) shouldBe 437
    Evaluator.evalRightToLeft(Token.tokenise("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")) shouldBe 12240
    Evaluator.evalRightToLeft(Token.tokenise("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")) shouldBe 13632
  }

  it("should prefer addition to multiplication") {
    Evaluator.evalPlusFirst(Token.tokenise("1 + 2 * 3")) shouldBe 9
  }

  it("should still add up properly") {
    Evaluator.evalPlusFirst(Token.tokenise("1 + 2 + 3 + 4")) shouldBe 10
  }

  it("should work with more varied examples") {
    Evaluator.evalPlusFirst(Token.tokenise("5 * 3 * 3 + (9 + 3 * 9 + 5) * 7 + 8")) shouldBe 5 * 3 * (3 + ((9 + 3) * (9 + 5))) * (7 + 8)
  }

  it("should do the second examples in the question") {
    Evaluator.evalPlusFirst(Token.tokenise("1 + 2 * 3 + 4 * 5 + 6")) shouldBe 231
    Evaluator.evalPlusFirst(Token.tokenise("1 + (2 * 3) + (4 * (5 + 6))")) shouldBe 51
    Evaluator.evalPlusFirst(Token.tokenise("5 + (8 * 3 + 9 + 3 * 4 * 3)")) shouldBe 1445
    Evaluator.evalPlusFirst(Token.tokenise("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")) shouldBe 669060
    Evaluator.evalPlusFirst(Token.tokenise("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")) shouldBe 23340
  }
}




