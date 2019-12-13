package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day05 extends SimpleCommonPuzzle[Vector[Int], Int, Int] {
  override def parse(resource: String): Vector[Int] =
    readResource(resource).flatMap(row => row.split(",").flatMap(raw => Try(raw.toInt).toOption)).toVector

  sealed trait Parameter
  case class ImmediateParameter(value: Int) extends Parameter
  case class PositionParameter(value: Int)  extends Parameter

  sealed trait Instruction
  case class   Addition       (p1: Parameter, p2: Parameter, p3: PositionParameter) extends Instruction
  case class   Multiplication (p1: Parameter, p2: Parameter, p3: PositionParameter) extends Instruction
  case class   Input          (p1: PositionParameter)                               extends Instruction
  case class   Output         (p1: Parameter)                                       extends Instruction
  case class   JumpIfTrue     (p1: Parameter, p2: Parameter)                        extends Instruction
  case class   JumpIfFalse    (p1: Parameter, p2: Parameter)                        extends Instruction
  case class   LessThan       (p1: Parameter, p2: Parameter, p3: PositionParameter) extends Instruction
  case class   Equals         (p1: Parameter, p2: Parameter, p3: PositionParameter) extends Instruction
  case object  Halt                                                                 extends Instruction

  def buildCurrentInstruction(code: Vector[Int])(current: Int): Instruction = {

    def extend(instruction: List[Int]): List[Int] = instruction match {
      case i if i.endsWith(List(9, 9)) => List(9, 9)
      case i if i.endsWith(List(1)) => leftPad(i)(0, 5)
      case i if i.endsWith(List(2)) => leftPad(i)(0, 5)
      case i if i.endsWith(List(3)) => leftPad(i)(0, 3)
      case i if i.endsWith(List(4)) => leftPad(i)(0, 3)
      case i if i.endsWith(List(5)) => leftPad(i)(0, 4)
      case i if i.endsWith(List(6)) => leftPad(i)(0, 4)
      case i if i.endsWith(List(7)) => leftPad(i)(0, 5)
      case i if i.endsWith(List(8)) => leftPad(i)(0, 5)
    }

    def leftPad[A](source: List[A])(pad: A, size: Int): List[A] =
      List.fill(size - source.size)(pad) ++ source

    def buildParameter(`type`: Int)(value: Int): Parameter = `type` match {
      case 0 => PositionParameter(value)
      case 1 => ImmediateParameter(value)
    }

    extend(toDigits(code(current))) match {
      case List(9, 9)            => Halt
      case List(0, p2, p1, 0, 1) => Addition(buildParameter(p1)(code(current + 1)), buildParameter(p2)(code(current + 2)), PositionParameter(code(current + 3)))
      case List(0, p2, p1, 0, 2) => Multiplication(buildParameter(p1)(code(current + 1)), buildParameter(p2)(code(current + 2)), PositionParameter(code(current + 3)))
      case List(0, 0, 3)         => Input(PositionParameter(code(current + 1)))
      case List(p1, 0, 4)        => Output(buildParameter(p1)(code(current + 1)))
      case List(p2, p1, 0, 5)    => JumpIfTrue(buildParameter(p1)(code(current + 1)), buildParameter(p2)(code(current + 2)))
      case List(p2, p1, 0, 6)    => JumpIfFalse(buildParameter(p1)(code(current + 1)), buildParameter(p2)(code(current + 2)))
      case List(0, p2, p1, 0, 7) => LessThan(buildParameter(p1)(code(current + 1)), buildParameter(p2)(code(current + 2)), PositionParameter(code(current + 3)))
      case List(0, p2, p1, 0, 8) => Equals(buildParameter(p1)(code(current + 1)), buildParameter(p2)(code(current + 2)), PositionParameter(code(current + 3)))
      case i => throw new IllegalArgumentException(s"Illegal instruction: $i")
    }
  }

  def solveProgram(intCode: Vector[Int])(input: Int): List[Int] = {
    def getValue(code: Vector[Int])(parameter: Parameter): Int = parameter match {
      case ImmediateParameter(value) => value
      case PositionParameter(value)  => code(value)
    }

    @tailrec
    def iterate(code: Vector[Int], current: Int, outputs: List[Int]): List[Int] = buildCurrentInstruction(code)(current) match {
      case Addition(p1, p2, PositionParameter(index))       => iterate(code.updated(index, getValue(code)(p1) + getValue(code)(p2)), current + 4, outputs)
      case Multiplication(p1, p2, PositionParameter(index)) => iterate(code.updated(index, getValue(code)(p1) * getValue(code)(p2)), current + 4, outputs)
      case Input(PositionParameter(index))                  => iterate(code.updated(index, input), current + 2, outputs)
      case Output(p1)                                       => iterate(code, current + 2, outputs :+ getValue(code)(p1))
      case JumpIfTrue(p1, p2)                               => iterate(code, if(getValue(code)(p1) != 0) getValue(code)(p2) else current + 3, outputs)
      case JumpIfFalse(p1, p2)                              => iterate(code, if(getValue(code)(p1) == 0) getValue(code)(p2) else current + 3, outputs)
      case LessThan(p1, p2, PositionParameter(index))       => iterate(code.updated(index, if(getValue(code)(p1) < getValue(code)(p2)) 1 else 0), current + 4, outputs)
      case Equals(p1, p2, PositionParameter(index))         => iterate(code.updated(index, if(getValue(code)(p1) == getValue(code)(p2)) 1 else 0), current + 4, outputs)
      case Halt                                             => outputs
    }

    iterate(intCode, 0, List())
  }

  def toDigits(n: Int): List[Int] =
    n.toString.toCharArray.map(_.asDigit).toList

  override def part1(intCode: Vector[Int]): Int =
    solveProgram(intCode)(1).last

  override def part2(intCode: Vector[Int]): Int =
    solveProgram(intCode)(5).last
}
