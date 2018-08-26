package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day06Definitions._
import com.lmat.util.Files.readResource

object Day06Definitions {
  case class Point(x: Int, y: Int)
  case class Instruction(action:String, c1: Point, c2: Point)
}

object Day06 extends SimpleCommonPuzzle[Seq[Instruction], Int, Int] {

  override def parse(resource: String): Seq[Instruction] =
    readResource(resource).flatMap(parseInstruction)

  def parseInstruction(row: String): Option[Instruction] = {
    val instruction = "(.*) ([0-9]*),([0-9]*) through ([0-9]*),([0-9]*)".r
    row match {
      case instruction(action, c1x, c1y, c2x, c2y) => Some(Instruction(action, Point(c1x.toInt, c1y.toInt), Point(c2x.toInt, c2y.toInt)))
      case _                                       => None
    }
  }

  override def part1(instructions: Seq[Instruction]): Int =
    points(1000).map(isLit(instructions)).count(identity)

  def points(n: Int): Seq[Point] =
    for {
      x <- 0 until n
      y <- 0 until n
    } yield Point(x, y)

  def isLit(instructions: Seq[Instruction])(point: Point): Boolean =
    instructions.foldLeft(false)(applyIfRelevant(point, changeState))

  def applyIfRelevant[T](point: Point, apply: (T, Instruction) => T)(state: T, instruction: Instruction): T = {
    def isRelevant(point: Point, instruction: Instruction): Boolean =
      contains(instruction.c1, instruction.c2, point)

    def contains(c1: Point, c2: Point, p: Point): Boolean = {
      val minX = c1.x min c2.x
      val maxX = c1.x max c2.x

      val minY = c1.y min c2.y
      val maxY = c1.y max c2.y

      minX <= p.x && p.x <= maxX &&
        minY <= p.y && p.y <= maxY
    }

    if (isRelevant(point, instruction)) apply(state, instruction) else state
  }

  def changeState(lit: Boolean, instruction: Instruction): Boolean =
    instruction.action match {
      case "turn on"  => true
      case "turn off" => false
      case "toggle"   => !lit
      case _          => throw new IllegalStateException("Illegal instruction")
  }

  override def part2(instructions: Seq[Instruction]): Int =
    points(1000).map(brightness(instructions)).sum

  def brightness(instructions: Seq[Instruction])(point: Point): Int =
    instructions.foldLeft(0)(applyIfRelevant(point, changeBrightness))

  def changeBrightness(brightness: Int, instruction: Instruction): Int =
    instruction.action match {
      case "turn on"  => brightness + 1
      case "turn off" => (brightness - 1) max 0
      case "toggle"   => brightness + 2
      case _          => throw new IllegalStateException("Illegal instruction")
    }
}
