package day8

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CpuSpec extends AnyFunSpec with Matchers {
  it("should compute a nop") {
    val program = Program.fromInput(List("nop +222"))
    val cpu = Cpu(program)
    val programState = ProgramState(0,0,Set.empty[Int], false, false)
    cpu.step(programState) shouldBe ProgramState(0,1,Set(0), false, false)
  }

  it("should compute an acc") {
    val program = Program.fromInput(List("acc +44"))
    val cpu = Cpu(program)
    val programState = ProgramState(0,0,Set.empty[Int], false, false)
    cpu.step(programState) shouldBe ProgramState(44,1,Set(0), false, false)
  }

  it("should compute a jump") {
    val program = Program.fromInput(List("jmp +44"))
    val cpu = Cpu(program)
    val programState = ProgramState(0,0,Set.empty[Int], false, false)
    cpu.step(programState) shouldBe ProgramState(0,44,Set(0), false, false)
  }

  it("should compute jumps relative to current position") {
    val program = Program.fromInput(List("nop +0","nop +0","nop +0","nop +0","jmp +5"))
    val cpu = Cpu(program)
    val programState = ProgramState(0,4,Set.empty[Int], false, false)
    cpu.step(programState) shouldBe ProgramState(0,9,Set(4), false, false)
  }

  it("should keep a record of previous instructions") {
    val program = Program.fromInput(List("nop +0","nop +0","nop +0"))
    val cpu = Cpu(program)
    val programState = ProgramState(0,1,Set(0), false, false)
    cpu.step(programState) shouldBe ProgramState(0,2,Set(0,1), false, false)
  }

  it("should detect a loop and not execute the instruction") {
    val program = Program.fromInput(List("acc +22", "nop +0", "jmp -2"))
    val cpu = Cpu(program)
    val programState = ProgramState(22,0,Set(0,1,2), false, false)
    cpu.step(programState) shouldBe ProgramState(22,0,Set(0,1,2), true, false)
  }

  it("should execute until loop detected") {
    val program = Program.fromInput(List("acc +22", "nop +0", "jmp -2"))
    val cpu = Cpu(program)
    val programState = ProgramState(0,0,Set.empty[Int], false, false)
    cpu.run(programState) shouldBe ProgramState(22,0,Set(0,1,2), true, false)
  }

  it("should execute until completion if no loop detected") {
    val program = Program.fromInput(List("acc +22", "nop +0", "nop +3", "acc +4"))
    val cpu = Cpu(program)
    val programState = ProgramState(0,0,Set.empty[Int], false, false)
    cpu.run(programState) shouldBe ProgramState(26,4,Set(0,1,2,3), false, true)
  }


}
