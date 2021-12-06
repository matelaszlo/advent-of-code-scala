package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2021.Day02Definitions._
import com.lmat.util.Files.readResource

object Day02Definitions {
  sealed trait Command
  case class Forward(units: Int) extends Command
  case class Up(units: Int)      extends Command
  case class Down(units: Int)    extends Command

  case class Position(horizontal: Int, depth: Int)
  case class Position2(horizontal: Int, depth: Int, aim: Int)
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

  override def part1(commands: List[Command]): Int = {
    val end = move(Position(0, 0), commands)
    end.horizontal * end.depth
  }

  def move(from: Position, commands: List[Command]): Position =
    commands.foldLeft(from)((position, command) => move(position, command))

  def move(position: Position, command: Command): Position = command match {
    case Forward(units) => position.copy(horizontal = position.horizontal + units)
    case Up(units)      => position.copy(depth = position.depth - units)
    case Down(units)    => position.copy(depth = position.depth + units)
  }

  override def part2(commands: List[Command]): Int = {
    val end = move2(Position2(0, 0, 0), commands)
    end.horizontal * end.depth
  }

  def move2(from: Position2, commands: List[Command]): Position2 =
    commands.foldLeft(from)((position, command) => move2(position, command))

  def move2(position: Position2, command: Command): Position2 = command match {
    case Forward(units) => position.copy(horizontal = position.horizontal + units, depth = position.depth + (position.aim * units))
    case Up(units)      => position.copy(aim = position.aim - units)
    case Down(units)    => position.copy(aim = position.aim + units)
  }
}
