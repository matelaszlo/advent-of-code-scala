package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day16Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day16Definitions {
  type Contraption = Map[Coordinate, Tile]

  case class Coordinate(x: Int, y: Int)
  object Coordinate {
    implicit val ordering: Ordering[Coordinate] = Ordering.by[Coordinate, (Int, Int)](c => (c.y, c.x))
  }

  sealed trait Tile
  case object MirrorRight extends Tile
  case object MirrorLeft extends Tile
  case object SplitterVertical extends Tile
  case object SplitterHorizontal extends Tile
  case object Empty extends Tile

  sealed trait Direction
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction

}
object Day16 extends SimpleCommonPuzzle[Contraption, Int, Int]{
  override def parse(resource: String): Contraption =
    parseContraption(readResource(resource).toList)

  def parseContraption(rows: List[String]): Contraption =
  rows.zipWithIndex
    .flatMap { case (row, y) => row.toCharArray.toList.zipWithIndex
      .map { case (c, x) => (Coordinate(x, y), parseTile(c)) }
    }
    .toMap

  def parseTile(c: Char): Tile = c match {
    case '/' => MirrorRight
    case '\\' => MirrorLeft
    case '|' => SplitterVertical
    case '-' => SplitterHorizontal
    case _ => Empty
  }

  def print(contraption: Contraption): String = {
    val maxX = contraption.keySet.map(_.x).max
    val maxY = contraption.keySet.map(_.y).max
    (0 to maxY)
      .map(y => (0 to maxX).map(x => contraption(Coordinate(x, y)) match {
        case MirrorRight => '/'
        case MirrorLeft => '\\'
        case SplitterVertical => '|'
        case SplitterHorizontal => '-'
        case Empty => '.'
      }).mkString("")).mkString("\n")
  }

  override def part1(contraption: Contraption): Int =
    energize(contraption, Coordinate(0,0), East).size

  def energize(contraption: Contraption, sC: Coordinate, sD: Direction): Set[Coordinate] = {
    def next(coordinate: Coordinate, direction: Direction): Set[(Coordinate, Direction)] = (contraption(coordinate), direction) match {
      case (Empty, _)                  => Set(head(coordinate, direction))
      case (MirrorRight, West)         => Set(head(coordinate, South))
      case (MirrorRight, North)        => Set(head(coordinate, East))
      case (MirrorRight, East)         => Set(head(coordinate, North))
      case (MirrorRight, South)        => Set(head(coordinate, West))
      case (MirrorLeft, West)          => Set(head(coordinate, North))
      case (MirrorLeft, North)         => Set(head(coordinate, West))
      case (MirrorLeft, East)          => Set(head(coordinate, South))
      case (MirrorLeft, South)         => Set(head(coordinate, East))
      case (SplitterVertical, North)   => Set(head(coordinate, North))
      case (SplitterVertical, South)   => Set(head(coordinate, South))
      case (SplitterVertical, West)    => Set(head(coordinate, North), head(coordinate, South))
      case (SplitterVertical, East)    => Set(head(coordinate, North), head(coordinate, South))
      case (SplitterHorizontal, North) => Set(head(coordinate, West),  head(coordinate, East))
      case (SplitterHorizontal, South) => Set(head(coordinate, West),  head(coordinate, East))
      case (SplitterHorizontal, West)  => Set(head(coordinate, West))
      case (SplitterHorizontal, East)  => Set(head(coordinate, East))
    }

    def head(coordinate: Coordinate, direction: Direction): (Coordinate, Direction) = direction match {
      case North => (Coordinate(coordinate.x, coordinate.y - 1), direction)
      case East  => (Coordinate(coordinate.x + 1, coordinate.y), direction)
      case South => (Coordinate(coordinate.x, coordinate.y + 1), direction)
      case West  => (Coordinate(coordinate.x - 1, coordinate.y), direction)
    }


    @tailrec
    def loop(seen: Set[(Coordinate, Direction)], current: Set[(Coordinate, Direction)]): Set[Coordinate] = {
//      println(s"Checking: $current Energized: $seen")
      if (current.isEmpty) seen.map(_._1)
      else {
        val n = current.flatMap { case (c, d) => next(c, d) }.filter { case (c, d) => contraption.contains(c) && !seen.contains((c, d)) }
        loop(seen ++ current, n)
      }
    }

    loop(Set.empty, Set((sC, sD)))
  }

  override def part2(contraption: Contraption): Int =
    starts(contraption).map { case (sC, sD) => energize(contraption, sC, sD).size }.max

  def starts(contraption: Contraption): Set[(Coordinate, Direction)] = {
    val maxX = contraption.keySet.map(_.x).max
    val maxY = contraption.keySet.map(_.y).max

    val rows = (0 to maxX).flatMap(x => List[(Coordinate, Direction)]((Coordinate(x, 0), South), (Coordinate(x, maxY), North))).toSet
    val columns = (0 to maxY).flatMap(y => List[(Coordinate, Direction)]((Coordinate(0, y), East), (Coordinate(maxX, y), West))).toSet
    rows ++ columns
  }
}
