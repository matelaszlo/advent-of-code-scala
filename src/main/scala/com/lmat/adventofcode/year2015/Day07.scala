package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day07Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day07Definitions {
  sealed trait Gate
  case class Identity(input: String)           extends Gate
  case class Not(input: String)                extends Gate
  case class LShift(input: String, value: Int) extends Gate
  case class RShift(input: String, value: Int) extends Gate

  case class And(left: String, right: String)  extends Gate
  case class Or(left: String, right: String)   extends Gate

  case class Instruction(input: Gate, output: String)
}

object Day07 extends SimpleCommonPuzzle[Seq[Instruction], Int, Int] {

  override def parse(resource: String): Seq[Instruction] = readResource(resource).flatMap(parseInstruction)

  def parseInstruction(row: String): Option[Instruction] = {
    val not      = "NOT (.*) -> (.*)".r
    val lshift   = "(.*) LSHIFT (.*) -> (.*)".r
    val rshift   = "(.*) RSHIFT (.*) -> (.*)".r
    val and      = "(.*) AND (.*) -> (.*)".r
    val or       = "(.*) OR (.*) -> (.*)".r
    val identity = "(.*) -> (.*)".r

    row match {
      case not(input, output)           => Some(Instruction(Not(input), output))
      case lshift(input, value, output) => Some(Instruction(LShift(input, value.toInt), output))
      case rshift(input, value, output) => Some(Instruction(RShift(input, value.toInt), output))
      case and(left, right, output)     => Some(Instruction(And(left, right), output))
      case or(left, right, output)      => Some(Instruction(Or(left, right), output))
      case identity(input, output)      => Some(Instruction(Identity(input), output))
      case _                            => None
    }
  }

  override def part1(instructions: Seq[Instruction]): Int = emulate(instructions).getOrElse("a", 0)

  /**
    * Algorithm:
    * Identify the instructions we can resolve (they either have constant or already resolved inputs)
    * Update the state with the output of these instructions
    * Iterate until all is resolved
    */
  def emulate(instructions: Seq[Instruction]): Map[String, Int] = {

    @tailrec
    def iterate(state: Map[String, Int], remaining: Seq[Instruction]): Map[String, Int] =
      if (remaining.isEmpty) state
      else {
        val (resolvable, rest) = remaining.partition(isResolvable(state))
        val updated = resolvable.foldLeft(state)(apply)
        iterate(updated, rest)
      }

    def isResolvable(state: Map[String, Int])(instruction: Instruction): Boolean = {
      def constantOrInMap(input: String): Boolean =
        isInt(input) || state.contains(input)

      instruction.input match {
        case Identity(input)  => constantOrInMap(input)
        case Not(input)       => constantOrInMap(input)
        case LShift(input, _) => constantOrInMap(input)
        case RShift(input, _) => constantOrInMap(input)
        case And(left, right) => constantOrInMap(left) && constantOrInMap(right)
        case Or(left, right)  => constantOrInMap(left) && constantOrInMap(right)
      }
    }

    def isInt(input: String): Boolean =
      Try(input.toInt).isSuccess

    def apply(state: Map[String, Int], instruction: Instruction): Map[String, Int] = {
      def constantOrFind(input: String): Int =
        if(isInt(input)) input.toInt else state(input)

      // Forcing the operation result with .toChar is to have the unsigned 16 bit functionality mentioned in the spec
      instruction.input match {
        case Identity(input)      => state.updated(instruction.output, constantOrFind(input).toChar)
        case Not(input)           => state.updated(instruction.output, (~ constantOrFind(input)).toChar)
        case LShift(input, value) => state.updated(instruction.output, (constantOrFind(input) << value).toChar)
        case RShift(input, value) => state.updated(instruction.output, (constantOrFind(input) >> value).toChar)
        case And(left, right)     => state.updated(instruction.output, (constantOrFind(left) & constantOrFind(right)).toChar)
        case Or(left, right)      => state.updated(instruction.output, (constantOrFind(left) | constantOrFind(right)).toChar)
      }
    }

    iterate(Map[String, Int](), instructions)
  }

  override def part2(instructions: Seq[Instruction]): Int = {
    val bValue = part1(instructions)
    val (b +: _, rest) = instructions.partition(_.output == "b")
    part1(b.copy(input = Identity(bValue.toString)) +: rest)
  }
}
