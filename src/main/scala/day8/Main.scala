package day8

import scala.io.Source

object Main extends App {

  val lines = Source.fromResource("consoleprogram.txt").getLines().toList
  val program = Program.fromInput(lines)
  val cpu = Cpu(program)
  val nonTerminatingState = cpu.run(ProgramState(0,0,Set.empty[Int],false, false)).accumulator

  val terminatingState = Program.findTerminatingProgram(program).accumulator

  println("Answer1: " + nonTerminatingState)
  println("Answer2: " + terminatingState)

}
