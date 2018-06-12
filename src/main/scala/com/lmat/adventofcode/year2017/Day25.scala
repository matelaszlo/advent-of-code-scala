package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2017.Day25Definitions._
import com.lmat.util.Files.readResource

object Day25Definitions {
  case class TuringMachineConfig(startState: String, steps: Int, states: Map[String, State])
  case class State(name: String, onZero: Instruction, onOne: Instruction)
  case class Instruction(write: Int, move: String, next: String)
}

object Day25 extends SimpleCommonPuzzle[TuringMachineConfig, Int, Unit] {
  override def parse(resource: String): TuringMachineConfig =
    parseInstructions(readResource(resource))

  def parseInstructions(lines: Seq[String]): TuringMachineConfig = {
    val firstLine = "Begin in state (.{1}).*".r
    val secondLine = "Perform a diagnostic checksum after (.*) steps.*".r

    def parseStartState(line: String): String = line match { case firstLine(state)  => state }
    def parseSteps(line:String)      : Int    = line match { case secondLine(state) => state.toInt }

    def parseState(lines: Seq[String]): State = {
      val name      = "In state (.{1}).*".r
      val value     = ".*Write the value (.{1}).{1}".r
      val direction = ".*Move one slot to the (.*).{1}".r
      val next      = ".*Continue with state (.{1}).{1}".r

      def parseName(line: String)     : String = line match {case name(state)      => state}
      def parseValue(line: String)    : Int    = line match {case value(state)     => state.toInt}
      def parseDirection(line: String): String = line match {case direction(state) => state}
      def parseNext(line: String)     : String = line match {case next(state)      => state}

      State(
        parseName(lines.head),
        Instruction(
          parseValue(lines(2)),
          parseDirection(lines(3)),
          parseNext(lines(4))
        ),
        Instruction(
          parseValue(lines(6)),
          parseDirection(lines(7)),
          parseNext(lines(8))
        ))
    }

    TuringMachineConfig(
      parseStartState(lines.head),
      parseSteps(lines.drop(1).head),
      lines.drop(3).sliding(10, 10).map(parseState).toList.groupBy(_.name).mapValues(_.head)
    )
  }

  case class TuringMachine(currentState: String, currentIndex:Int, switchedOn: Set[Int])

  override def part1(turingMachineConfig: TuringMachineConfig): Int =
    runTuringMachine(turingMachineConfig).switchedOn.size

  def runTuringMachine(turingMachineConfig: TuringMachineConfig): TuringMachine =
    (0 until turingMachineConfig.steps).foldLeft(TuringMachine(turingMachineConfig.startState, 0, Set()))((state, step) => nextState(turingMachineConfig)(state, step))

  def nextState(turingMachineConfig: TuringMachineConfig)(turingMachine: TuringMachine, step: Int): TuringMachine = {
    val currentValueIsOne = turingMachine.switchedOn.contains(turingMachine.currentIndex)
    val currentValue = if (currentValueIsOne) 1 else 0
    val currentState = turingMachineConfig.states(turingMachine.currentState)
    val currentInstruction = if(currentValueIsOne) currentState.onOne else currentState.onZero

    val nextState = currentInstruction.next

    val nextIndex =
      currentInstruction.move match {
        case "left"  => turingMachine.currentIndex - 1
        case "right" => turingMachine.currentIndex + 1
      }

    val nextSwitchedOn =
      if (currentInstruction.write == currentValue) turingMachine.switchedOn
      else if(currentInstruction.write == 1) turingMachine.switchedOn + turingMachine.currentIndex
      else turingMachine.switchedOn - turingMachine.currentIndex

    TuringMachine(nextState, nextIndex, nextSwitchedOn)
  }

  override def part2(turingMachineConfig: TuringMachineConfig): Unit = ()
}
