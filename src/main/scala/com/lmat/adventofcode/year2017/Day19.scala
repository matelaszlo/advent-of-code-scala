package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimplePuzzle
import com.lmat.adventofcode.year2017.Day19Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day19Definitions {
  case class Position(row: Int, column: Int)

  sealed trait Tile
  case object  Empty                  extends Tile
  case object  VerticalPath           extends Tile
  case object  HorizontalPath         extends Tile
  case object  Crossing               extends Tile
  case class   CheckPoint(name: Char) extends Tile
}

object Day19 extends SimplePuzzle[Map[Position, Tile], String, Int] {
  override def parse(resource: String): Map[Position, Tile] = parseMap(readResource(resource).map(_.toSeq))

  def parseMap(input: Seq[Seq[Char]]): Map[Position, Tile] = {
    val rowsNum    = input.size
    val columnsNum = input.map(_.size).max

    (for {
      row    <- 0 until rowsNum
      column <- 0 until columnsNum
      tile   <- parseTile(input.lift(row).flatMap(_.lift(column)).getOrElse(' '))
    } yield (Position(row, column), tile)).toMap
  }

  def parseTile(char: Char): Option[Tile]  = char match {
    case '|' => Some(VerticalPath)
    case '-' => Some(HorizontalPath)
    case '+' => Some(Crossing)
    case ' ' => None
    case c   => Some(CheckPoint(c))
  }

  override def part1(tiles: Map[Position, Tile]): String = {
    @tailrec
    def checkpoints(current: Position, heading: String, acc: String): String =
      (tiles.getOrElse(current, Empty), next(tiles, current, heading)) match {
        case (CheckPoint(c), None) => acc + c
        case (_,             None) => acc
        case (CheckPoint(c), Some((nextPos, nextHead))) => checkpoints(nextPos, nextHead, acc + c)
        case (_,             Some((nextPos, nextHead))) => checkpoints(nextPos, nextHead, acc)
      }

    checkpoints(findStartPosition(tiles).get, "down", "")
  }

  def next(tiles: Map[Position, Tile], current: Position, heading: String): Option[(Position, String)] = {
    val left  = Position(current.row, current.column - 1)
    val right = Position(current.row, current.column + 1)
    val up    = Position(current.row - 1, current.column)
    val down  = Position(current.row + 1, current.column)

    val goLeft  = Some((left,  "left"))
    val goRight = Some((right, "right"))
    val goUp    = Some((up,    "up"))
    val goDown  = Some((down,  "down"))

    (tiles.getOrElse(current, Empty), heading) match {
      case (Empty,    _)       => None

      case (Crossing, "down")  => if (tiles.getOrElse(left, Empty) != Empty) goLeft else goRight
      case (Crossing, "up")    => if (tiles.getOrElse(left, Empty) != Empty) goLeft else goRight
      case (Crossing, "left")  => if (tiles.getOrElse(up,   Empty) != Empty) goUp   else goDown
      case (Crossing, "right") => if (tiles.getOrElse(up,   Empty) != Empty) goUp   else goDown

      case (_,        "down")  => goDown
      case (_,        "up")    => goUp
      case (_,        "left")  => goLeft
      case (_,        "right") => goRight
    }
  }

  def findStartPosition(tiles: Map[Position, Tile]): Option[Position] = tiles.find {
    case (p, VerticalPath) => p.row == 0
    case (_, _)            => false
  }.map(_._1)

  override def part2(tiles: Map[Position, Tile]): Int =  {

    @tailrec
    def countSteps(current: Position, heading: String, acc: Int): Int =
      next(tiles, current, heading) match {
        case None => acc
        case Some((nextPos, nextHead)) => countSteps(nextPos, nextHead, acc + 1)
      }

    countSteps(findStartPosition(tiles).get, "down", 0)
  }
}
