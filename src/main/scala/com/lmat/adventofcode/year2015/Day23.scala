package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day23Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day23Definitions {
  sealed trait Instruction
  case class Half(register: String)                    extends Instruction
  case class Triple(register: String)                  extends Instruction
  case class Increment(register: String)               extends Instruction
  case class Jump(offset: Int)                         extends Instruction
  case class JumpIfEven(register: String, offset: Int) extends Instruction
  case class JumpIfOdd(register: String, offset: Int)  extends Instruction
}

object Day23 extends SimpleCommonPuzzle[Seq[Instruction], Int, Int]{
  override def parse(resource: String): Seq[Instruction] = readResource(resource).flatMap(parseInstruction)

  def parseInstruction(row: String): Option[Instruction] = {
    val half       = "hlf (.*)".r
    val triple     = "tpl (.*)".r
    val increment  = "inc (.*)".r
    val jump       = "jmp (.*)".r
    val jumpIfEven = "jie (.*?), (.*)".r
    val jumpIfOdd  = "jio (.*?), (.*)".r

    row match {
      case half(register)               => Some(Half(register))
      case triple(register)             => Some(Triple(register))
      case increment(register)          => Some(Increment(register))
      case jump(offset)                 => Try(offset.toInt).map(Jump).toOption
      case jumpIfEven(register, offset) => Try(offset.toInt).map(JumpIfEven(register, _)).toOption
      case jumpIfOdd(register, offset)  => Try(offset.toInt).map(JumpIfOdd(register, _)).toOption
      case _                            => None
    }
  }

  case class State(registers: Map[String, Int], current: Int) {
    def test(register: String, f: Int => Boolean): Boolean = registers.get(register).exists(f)
    def updated(register: String, f: Int => Int): State = registers.get(register).map(value => State(registers.updated(register, f(value)), current)).getOrElse(this)
    def jump(offset: Int): State = State(registers, current + offset)
    def next: State = jump(1)
  }

  override def part1(instructions: Seq[Instruction]): Int =
    applyInstructions(State(Map("a" -> 0, "b" -> 0), 0), instructions).registers("b")

  @tailrec
  def applyInstructions(state: State, instructions: Seq[Instruction]): State =
    if(state.current < 0 || state.current >= instructions.size) state
    else applyInstructions(applyInstruction(state, instructions(state.current)), instructions)

  def applyInstruction(state: State, instruction: Instruction): State = instruction match {
    case Half(register)               => state.updated(register, _ / 2).next
    case Triple(register)             => state.updated(register, _ * 3).next
    case Increment(register)          => state.updated(register, _ + 1).next
    case Jump(offset)                 => state.jump(offset)
    case JumpIfEven(register, offset) => if(state.test(register, _ % 2 == 0)) state.jump(offset) else state.next
    case JumpIfOdd(register, offset)  => if(state.test(register, _ == 1)) state.jump(offset) else state.next
  }

  override def part2(instructions: Seq[Instruction]): Int =
    applyInstructions(State(Map("a" -> 1, "b" -> 0), 0), instructions).registers("b")
}
