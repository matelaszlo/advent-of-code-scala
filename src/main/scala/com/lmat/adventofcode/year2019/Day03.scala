package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.adventofcode.year2019.Day03Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Traverse

import scala.util.Try

object Day03Definitions {
  sealed trait Direction
  case object Up    extends Direction
  case object Down  extends Direction
  case object Right extends Direction
  case object Left  extends Direction

  case class Section(direction: Direction, length: Int)

  type Wire = List[Section]

  case class Point(x: Int, y: Int)
  lazy val center = Point(0, 0)
}

object Day03 extends CommonPuzzle[List[Wire], (List[Point], List[Point]), Int, Int] {
  override def parse(resource: String): List[Wire] =
    readResource(resource).flatMap(parseWire).toList

  def parseWire(row: String): Option[Wire] =
    Traverse.sequenceOption(row.split(",").toIndexedSeq.map(parseSection)).map(_.toList)

  def parseSection(raw: String): Option[Section] = {
    def extractSection(chars: Seq[Char], direction: Direction): Option[Section] =
      Try(chars.mkString.toInt).toOption.map(length => Section(direction, length))

    raw.toIndexedSeq match {
      case 'U' +: num => extractSection(num, Up)
      case 'D' +: num => extractSection(num, Down)
      case 'R' +: num => extractSection(num, Right)
      case 'L' +: num => extractSection(num, Left)
      case _ => None
    }
  }

  override def preProcess(wires: List[Wire]): (List[Point], List[Point]) = {
    val wire1 :: wire2 :: _ = wires
    (wireToPoints(wire1), wireToPoints(wire2))
  }

  def wireToPoints(wire: Wire): List[Point] = {
    def sectionToPoints(start: Point, section: Section): List[Point] = section.direction match {
      case Day03Definitions.Up    => (start.y + 1 to start.y + section.length).map(y => Point(start.x, y)).toList
      case Day03Definitions.Down  => (start.y - section.length until start.y).reverse.map(y => Point(start.x, y)).toList
      case Day03Definitions.Right => (start.x + 1 to start.x + section.length).map(x => Point(x, start.y)).toList
      case Day03Definitions.Left  => (start.x - section.length until start.x).reverse.map(x => Point(x, start.y)).toList
    }

    wire.foldLeft(List(center))((points, section) => points ++ sectionToPoints(points.last, section))
  }

  override def part1(wires: (List[Point], List[Point])): Int = {
    val (wire1, wire2) = wires
    val intersections = (wire1.toSet intersect wire2.toSet) - center
    intersections.map(manhattan(center)).min
  }

  def manhattan(p1: Point)(p2: Point): Int =
    Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

  override def part2(wires: (List[Point], List[Point])): Int = {
    val (wire1, wire2) = wires
    val intersections = (wire1.toSet intersect wire2.toSet) - center
    intersections.map(i => wire1.indexOf(i) + wire2.indexOf(i)).min
  }
}
