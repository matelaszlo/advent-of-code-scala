package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day01 extends SimpleCommonPuzzle[Seq[Char], Int, Int] {
  override def parse(resource: String): Seq[Char] = readResource(resource).flatMap(_.toCharArray)

  override def part1(instructions: Seq[Char]): Int =
    instructions.foldLeft(0)(move)

  override def part2(instructions: Seq[Char]): Int =
    instructions.scanLeft(0)(move)
      .zipWithIndex
      .find { case (f, _) => f == -1 }
      .map(_._2).get

  def move(floor: Int, char: Char): Int = char match {
    case '(' => floor + 1
    case ')' => floor - 1
    case _   => floor
  }
}
