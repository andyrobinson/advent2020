package day8

case class ProgramState(accumulator: Int, programCounter: Int, executed: Set[Int], loopDetected: Boolean, terminated: Boolean)

case class Instruction(op: String, param: Int)

case class Program(var instructions: Array[Instruction]) {
  def findTerminatingProgram: ProgramState = {
    var currentIndex = 0
    while (currentIndex < instructions.size) {
      if (instructions(currentIndex).op != "acc") {
        val originalInstruction = instructions(currentIndex)
        instructions(currentIndex) = swap(originalInstruction)
        val cpu = Cpu(this)
        val result = cpu.run(ProgramState(0,0,Set.empty[Int], false, false))
        if (result.terminated) return result
        instructions(currentIndex) = originalInstruction
      }
      currentIndex = currentIndex + 1
    }
    throw new RuntimeException("no solution found")
  }

  private def swap(instruction: Instruction): Instruction =
    if (instruction.op == "jmp") instruction.copy(op = "nop")
    else instruction.copy(op = "jmp")
}

object Program {

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
