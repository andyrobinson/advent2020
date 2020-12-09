package day8

case class Cpu(program: Program) {
  def step(oldState: ProgramState): ProgramState = {
    if (oldState.executed.contains(oldState.programCounter)) return oldState.copy(loopDetected = true)
    if (oldState.programCounter >= program.instructions.size) return oldState.copy(terminated = true)

    program.instructions(oldState.programCounter) match {
      case Instruction("nop",_) => ProgramState(oldState.accumulator, oldState.programCounter + 1, oldState.executed + oldState.programCounter, false, false)
      case Instruction("acc", value) => ProgramState(oldState.accumulator + value, oldState.programCounter + 1, oldState.executed + oldState.programCounter, false, false)
      case Instruction("jmp", value) => ProgramState(oldState.accumulator, oldState.programCounter + value, oldState.executed + oldState.programCounter, false, false)
      case _ => throw new NotImplementedError("oops")
    }
  }

  def run(programState: ProgramState) : ProgramState = {
    if (programState.loopDetected || programState.terminated) programState
    else run(step(programState))
  }
}
