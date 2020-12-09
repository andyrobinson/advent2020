package day8

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ProgramSpec extends AnyFunSpec with Matchers {
  it("should parse instructions") {
    val lines = List("nop +9", "acc +47", "jmp -200")
    Program.fromInput(lines).instructions shouldBe List(Instruction("nop",9),Instruction("acc", 47), Instruction("jmp",-200)).toArray
  }
}
