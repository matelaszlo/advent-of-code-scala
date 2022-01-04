package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2021.Day05Definitions._
import com.lmat.util.Files.readResource

object Day05Definitions {
  case class Coordinate(x: Int, y: Int)
  case class Line(from: Coordinate, to: Coordinate)
}

object Day05 extends SimpleCommonPuzzle[List[Line], Int, Int] {
  override def parse(resource: String): List[Line] =
    readResource(resource).flatMap(parseLine).toList

  def parseLine(raw: String): Option[Line] = {
    val line = s"(.*),(.*) -> (.*),(.*)".r
    raw match {
      case line(x1Raw, y1Raw, x2Raw, y2Raw) =>
        for {
          x1 <- x1Raw.toIntOption
          y1 <- y1Raw.toIntOption
          x2 <- x2Raw.toIntOption
          y2 <- y2Raw.toIntOption
        } yield Line(Coordinate(x1, y1), Coordinate(x2, y2))
      case _ => None
    }
  }

  override def part1(lines: List[Line]): Int = {
    val pointsMap = toPointsMap(lines.filter(line => horizontal(line) || vertical(line)))
//    println(draw(pointsMap))
    pointsMap.count{ case (_, count) => count >= 2}
  }

  override def part2(lines: List[Line]): Int = {
    val pointsMap = toPointsMap(lines)
//    println(draw(pointsMap))
    pointsMap.count{ case (_, count) => count >= 2}
  }

  def toPointsMap(lines: List[Line]): Map[Coordinate, Int] =
    lines.flatMap(toPoints)
      .groupBy(identity).view.mapValues(_.size).toMap

  def horizontal(line: Line): Boolean =
    line.from.y == line.to.y

  def vertical(line: Line): Boolean =
    line.from.x == line.to.x

  def diagonal(line: Line): Boolean =
    Math.abs(line.from.x - line.to.x) == Math.abs(line.from.y - line.to.y)

  // Invariant: Line is either horizontal or vertical or 45 degrees diagonal
  def toPoints(line: Line): List[Coordinate] = {
    // We calculate how much we need to change each coordinate in each step based on the from and to points in the line
    val xDelta = -line.from.x.compare(line.to.x)
    val yDelta = -line.from.y.compare(line.to.y)

    def next(coordinate: Coordinate): Coordinate =
      coordinate.copy(x = coordinate.x + xDelta, y = coordinate.y + yDelta)

    // We iterate until we reach the endpoint and append it
    LazyList.iterate(line.from)(next).takeWhile(_ != line.to).toList :+ line.to
  }

  def draw(pointsMap: Map[Coordinate, Int]): String = {
    val maxX = pointsMap.keys.map(_.x).max
    val maxY = pointsMap.keys.map(_.y).max
    (0 to maxY).map(y => (0 to maxX).flatMap(x =>
      pointsMap.get(Coordinate(x, y)) match {
        case Some(count) => count.toString.toCharArray.toList
        case None        => List('.')
      }).mkString("", "", "\n")
    ).mkString
  }
}
