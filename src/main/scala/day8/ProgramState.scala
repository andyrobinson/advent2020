package day8

case class ProgramState(accumulator: Int, programCounter: Int, executed: Set[Int], loopDetected: Boolean, terminated: Boolean)

case class Instruction(op: String, param: Int)

case class Program(instructions: Array[Instruction])

object Program {

  def findTerminatingProgram(program: Program, currentIndex: Int = 0): ProgramState = {
    val instructions = program.instructions
    if (currentIndex > instructions.size) throw new RuntimeException("no solution found")
    else {
      if (instructions(currentIndex).op == "acc") findTerminatingProgram(program, currentIndex + 1) // no mutation of this instruction
      else {
        val originalInstruction = instructions(currentIndex)
        val newInstructions: Array[Instruction] = (instructions.take(currentIndex) :+ swap(originalInstruction)) ++ instructions.takeRight(instructions.size - (currentIndex + 1))
        val cpu = Cpu(Program(newInstructions))
        val result = cpu.run(ProgramState(0,0,Set.empty[Int], false, false))
        if (result.terminated) result
        else findTerminatingProgram(program, currentIndex + 1)
      }
    }
  }

  private def swap(instruction: Instruction): Instruction =
    if (instruction.op == "jmp") instruction.copy(op = "nop")
    else instruction.copy(op = "jmp")

  def fromInput(lines: List[String]): Program = {
    val instructionRegEx = """^(.+)\s([\\+|-][0-9]+)$""".r
    val instructions = lines.map { line =>
      line match {
        case instructionRegEx(op, param) => Instruction(op, param.toInt)
        case _ => throw new RuntimeException("cannot parse instruction")
      }
    }
    Program(instructions.toArray)
  }
}
