package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2017.Day08Definitions._
import com.lmat.util.Files.readResource

import scala.collection.immutable.Map

object Day08Definitions {
  case class Condition(register: String, comparison: String, amount: Int)
  case class Instruction(register: String, operation: String, amount: Int, condition: Condition)
}

object Day08 extends SimpleCommonPuzzle[Seq[Instruction], Int, Int] {
  override def parse(resource: String): Seq[Instruction] =
    readResource(resource).map(parseInstruction)

  def parseInstruction(string: String): Instruction = {
    val components = string.split("\\s+")
    Instruction(
      components(0),
      components(1),
      components(2).toInt,
      Condition(
        components(4),
        components(5),
        components(6).toInt
      )
    )
  }

  case class RegisterState(state: Map[String, Int]) {
    def get(register: String): Int =
      state.getOrElse(register, 0)

    def inc(register: String, amount:Int) =
      RegisterState(state.updated(register, get(register) + amount))

    def dec(register: String, amount:Int) =
      RegisterState(state.updated(register, get(register) - amount))

    def maxValue: Int =
      if (state.isEmpty) 0
      else state.values.max
  }

  object RegisterState {
    val empty = RegisterState(Map(): Map[String, Int])
  }

  override def part1(instructions: Seq[Instruction]): Int =
    instructions.foldLeft(RegisterState.empty)((state, instruction) => applyInstruction(state, instruction))
      .maxValue

  override def part2(instructions: Seq[Instruction]): Int =
    instructions.scanLeft(RegisterState.empty)((state, instruction) => applyInstruction(state, instruction))
      .map(_.maxValue).max

  def applyInstruction(registerState: RegisterState, instruction: Instruction): RegisterState = (instruction.operation, evaluateCondition(registerState, instruction.condition)) match {
    case (_,     false) => registerState
    case ("inc", true)  => registerState.inc(instruction.register, instruction.amount)
    case ("dec", true)  => registerState.dec(instruction.register, instruction.amount)
  }

  def evaluateCondition(registerState: RegisterState, condition: Condition): Boolean = condition.comparison match {
    case "==" => registerState.get(condition.register) == condition.amount
    case "!=" => registerState.get(condition.register) != condition.amount
    case "<"  => registerState.get(condition.register) <  condition.amount
    case "<=" => registerState.get(condition.register) <= condition.amount
    case ">"  => registerState.get(condition.register) >  condition.amount
    case ">=" => registerState.get(condition.register) >= condition.amount
  }
}
