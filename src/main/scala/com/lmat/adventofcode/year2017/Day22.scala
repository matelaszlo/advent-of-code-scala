package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2017.Day22Definitions._
import com.lmat.util.Files.readResource

object Day22Definitions {
  case class Position(x: Int, y: Int)
}

object Day22 extends SimpleCommonPuzzle[Map[Position, String], Int, Int] {
  override def parse(resource: String): Map[Position, String] = parseMap(readResource(resource).map(_.toCharArray.toSeq))

  /**
    * Note here the transformation from (row, column) to (x, y) coordinates
    * (0, 0) to mean the middle of the input grid
    */
  def parseMap(input: Seq[Seq[Char]]): Map[Position, String] = {
    val size   = input.size
    val middle = size / 2
    (for {
      x <- 0 until size
      y <- 0 until size
    } yield (Position(y - middle, middle - x), if (input(x)(y) == '#') "infected" else "clean")).toMap
  }

  override def part1(startingMap: Map[Position, String]): Int =
    countInfectiousBursts(startingMap, decideOnDirection, decideNodeState,  10000)

  case class State(map: Map[Position, String], position: Position, facing: String, infectedCount: Int)

  def countInfectiousBursts(startingMap: Map[Position, String], directionDecider: String => String, nodeStateDecider: String => String, bursts: Int): Int =
    (0 until bursts)
      .foldLeft(State(startingMap, Position(0, 0), "up", 0)) { case (state, _) => nextState(state, directionDecider, nodeStateDecider) }
      .infectedCount

  def nextState(state: State, directionDecider: String => String, nodeStateDecider: String => String): State = {
    val nodeState = state.map.getOrElse(state.position, "clean")
    val newNodeState = nodeStateDecider(nodeState)

    val facing = turn(state.facing, directionDecider(nodeState))
    val map = state.map.updated(state.position, newNodeState)
    val position = move(state.position, facing)
    val infectedCount = if(newNodeState == "infected") state.infectedCount + 1 else state.infectedCount

    State(map, position, facing, infectedCount)
  }

  def turn(facing: String, direction:String): String = (facing, direction) match {
    case ("up", "left")       => "left"
    case ("up", "right")      => "right"
    case ("up", "no")         => "up"
    case ("up", "reverse")    => "down"

    case ("right", "left")    => "up"
    case ("right", "right")   => "down"
    case ("right", "no")      => "right"
    case ("right", "reverse") => "left"

    case ("down", "left")     => "right"
    case ("down", "right")    => "left"
    case ("down", "no")       => "down"
    case ("down", "reverse")  => "up"

    case ("left", "left")     => "down"
    case ("left", "right")    => "up"
    case ("left", "no")       => "left"
    case ("left", "reverse")  => "right"
    case (_,        _)        => throw new IllegalArgumentException("Wrong state")
  }

  def move(position: Position, facing: String): Position = facing match {
    case "up"    => Position(position.x,     position.y + 1)
    case "right" => Position(position.x + 1, position.y)
    case "down"  => Position(position.x,     position.y - 1)
    case "left"  => Position(position.x - 1, position.y)
  }

  def decideOnDirection(nodeState: String): String = nodeState match {
    case "infected" => "right"
    case "clean"    => "left"
  }

  def decideNodeState(nodeState: String): String = nodeState match {
    case "infected" => "clean"
    case "clean"    => "infected"
  }

  override def part2(startingMap: Map[Position, String]): Int =
    countInfectiousBursts(startingMap, decideOnDirectionV2, decideNodeStateV2, 10000000)

  def decideOnDirectionV2(nodeState: String): String = nodeState match {
    case "infected" => "right"
    case "flagged"  => "reverse"
    case "clean"    => "left"
    case "weakened" => "no"
  }
  def decideNodeStateV2(nodeState: String): String = nodeState match {
    case "infected" => "flagged"
    case "flagged"  => "clean"
    case "weakened" => "infected"
    case "clean"    => "weakened"
  }
}
