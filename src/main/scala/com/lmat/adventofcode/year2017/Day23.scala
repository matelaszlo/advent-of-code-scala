package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2017.Day23Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Maths.isComposite

import scala.annotation.tailrec
import scala.util.Try

object Day23Definitions {
  sealed trait Instruction
  case class Set      (base: String, value: String) extends Instruction
  case class Subtract (base: String, value: String) extends Instruction
  case class Multiply (base: String, value: String) extends Instruction
  case class Jump     (base: String, value: String) extends Instruction
}

object Day23 extends SimpleCommonPuzzle[Seq[Instruction], Int, Int] {
  override def parse(resource: String): Seq[Instruction] = readResource(resource).flatMap(parseInstruction).toVector

  def parseInstruction(line: String): Option[Instruction] = {
    val setTest       = """set\s+(.+)\s+(.+)""".r
    val subtractTest  = """sub\s+(.+)\s+(.+)""".r
    val multiplyTest  = """mul\s+(.+)\s+(.+)""".r
    val jumpTest      = """jnz\s+(.+)\s+(.+)""".r

    line match {
      case setTest(base, value)       => Some(Set(base, value))
      case subtractTest(base, value)  => Some(Subtract(base, value))
      case multiplyTest(base, value)  => Some(Multiply(base, value))
      case jumpTest(base, value)      => Some(Jump(base, value))
      case _                          => None
    }
  }

  override def part1(instructions: Seq[Instruction]): Int = countMultiplies(State(Map(), 0, 0), instructions)

  case class State(registers: Map[String, Long], current: Int, multiplies: Int) {
    def register(name: String): Long = Try(name.toLong).getOrElse(registers.getOrElse(name, 0))

    def update(name: String, value: String): State = copy(registers = registers.updated(name, register(value)))
    def update(name: String, value: String, function: (Long, Long) => Long): State = copy(registers = registers.updated(name, function(register(name), register(value))))

    def next: State = copy(current = current + 1)
    def jump(value: String): State = copy(current = current + register(value).toInt)
  }

  @tailrec
  def countMultiplies(state: State, instructions: Seq[Instruction]): Int =
    if (isFinished(state, instructions)) state.multiplies
    else countMultiplies(applyInstruction(state, instructions(state.current)), instructions)

  def isFinished(state: State, instructions: Seq[Instruction]): Boolean = state.current < 0 || state.current >= instructions.size

  def applyInstruction(state: State, instruction: Instruction): State = instruction match {
    case Set     (base, value)  => state.update(base, value).next
    case Subtract(base, value)  => state.update(base, value, _ - _).next
    case Multiply(base, value)  => state.update(base, value, _ * _).next.copy(multiplies = state.multiplies + 1)
    case Jump    (base, value)  => if(state.register(base) != 0)  state.jump(value)  else state.next
  }

  /**
    * This one is a very interesting challange
    * Simulating it is out of the question as it translates to 3 large nested loops
    * If you stare at the assembly instructions long enough and inspect the state at strategic places you can figure out that the program is essentially
    * Counting the composite numbers it encounters going `from` a large number `to` another one in `increments` defined by your input
    */
  override def part2(instructions: Seq[Instruction]): Int = {
    val (start, end, increments) = findKeyNumbers(instructions)
    (start to end by increments).count(isComposite)
  }

  /**
    * Just to be fancy we can extract the key numbers from the instructions
    * The instructions are the same for everyone just a few key numbers change
    */
  def findKeyNumbers(instructions: Seq[Instruction]): (Int, Int, Int) = {
    val base     = instructions.head.asInstanceOf[Set].value.toInt    // set b 99 => 99
    val multiply = instructions(4).asInstanceOf[Multiply].value.toInt // mul b 100 => 100
    val sub      = instructions(5).asInstanceOf[Subtract].value.toInt // sub b -100000 => -100000
    val diff     = instructions(7).asInstanceOf[Subtract].value.toInt // sub c -17000  => -17000

    val start      = base * multiply - sub
    val end        = start - diff
    val increments = Math.abs(instructions(30).asInstanceOf[Subtract].value.toInt) // sub b -17 => 17
    (start, end, increments)
  }
}
