package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2021.Day02Definitions._
import com.lmat.util.Files.readResource

object Day02Definitions {
  sealed trait Command
  case class Forward(units: Int) extends Command
  case class Up(units: Int)      extends Command
  case class Down(units: Int)    extends Command

  case class Position(horizontal: Int, depth: Int, aim: Int)
}

object Day02 extends SimpleCommonPuzzle[List[Command], Int, Int] {
  override def parse(resource: String): List[Command] =
    readResource(resource).flatMap(parseCommand).toList

  def parseCommand(row: String): Option[Command] = {
    val forward = s"forward (.*)".r
    val up      = s"up (.*)".r
    val down    = s"down (.*)".r

    row match {
      case forward(units) => units.toIntOption.map(Forward)
      case up(units)      => units.toIntOption.map(Up)
      case down(units)    => units.toIntOption.map(Down)
      case _              => None
    }
  }

  val start: Position = Position(0, 0, 0)

  override def part1(commands: List[Command]): Int = {
    val end = commands.foldLeft(start)(move1)
    end.horizontal * end.depth
  }

  override def part2(commands: List[Command]): Int = {
    val end = commands.foldLeft(start)(move2)
    end.horizontal * end.depth
  }

  def move1(position: Position, command: Command): Position = command match {
    case Forward(units) => position.copy(horizontal = position.horizontal + units)
    case Up(units)      => position.copy(depth = position.depth - units)
    case Down(units)    => position.copy(depth = position.depth + units)
  }

  def move2(position: Position, command: Command): Position = command match {
    case Forward(units) => position.copy(horizontal = position.horizontal + units, depth = position.depth + (position.aim * units))
    case Up(units)      => position.copy(aim = position.aim - units)
    case Down(units)    => position.copy(aim = position.aim + units)
  }
}
