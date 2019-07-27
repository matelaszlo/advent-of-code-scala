package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import scala.language.postfixOps

object Day03 extends SimpleCommonPuzzle[Seq[Char], Int, Int] {
  override def parse(resource: String): Seq[Char] = readResource(resource).flatMap(_.toCharArray)

  case class Position(x: Int, y: Int)

  override def part1(directions: Seq[Char]): Int =
    visited(directions).distinct.size

  def visited(directions: Seq[Char]): Seq[Position] =
    directions.scanLeft(Position(0, 0))(move)

  def move(position: Position, char: Char): Position = char match {
    case '^' => position.copy(y = position.y + 1)
    case 'v' => position.copy(y = position.y - 1)
    case '>' => position.copy(x = position.x + 1)
    case '<' => position.copy(x = position.x - 1)
    case _   => position
  }

  override def part2(directions: Seq[Char]): Int = {
    val (original, robo) = directions.zipWithIndex.partition(_._2 % 2 == 0)
    visited(original.map(_._1)).toSet union visited(robo.map(_._1)).toSet size
  }
}
