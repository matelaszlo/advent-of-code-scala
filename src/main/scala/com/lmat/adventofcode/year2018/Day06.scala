package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2018.Day06Definitions.Coordinate
import com.lmat.util.Files.readResource

import scala.util.Try

object Day06Definitions {
  case class Coordinate(x: Int, y: Int)
}

object Day06 extends SimpleCommonPuzzle[Seq[Coordinate], Int, Int] {
  override def parse(resource: String): Seq[Coordinate] = readResource(resource).flatMap(parseCoordinate)

  def parseCoordinate(row: String): Option[Coordinate] = {
    val pattern = "(.*?), (.*)".r
    row match {
      case pattern(xS, yS) => (for {
        x <- Try(xS.toInt)
        y <- Try(yS.toInt)
      } yield Coordinate(x, y)).toOption
      case _ => None
    }
  }

  override def part1(coordinates: Seq[Coordinate]): Int = {
    val (min, max) = defineSquare(coordinates)
    squareCoordinates(min, max).groupBy(closestIndex(_, coordinates)).values
      .filterNot(containsEdge(_, min, max))
      .map(_.size).max
  }

  def containsEdge(coordinates: Seq[Coordinate], min: Coordinate, max: Coordinate): Boolean =
    coordinates.exists(c => c.x == min.x || c.x == max.x || c.y == min.y || c.y == max.y)

  def closestIndex(c: Coordinate, coordinates: Seq[Coordinate]): Int =
    coordinates.zipWithIndex.minBy{case (current, _) => manhattan(c, current)}._2

  def squareCoordinates(min: Coordinate, max: Coordinate): Seq[Coordinate] = for {
      x <- min.x to max.x
      y <- min.y to max.y
    } yield Coordinate(x, y)

  def defineSquare(coordinates: Seq[Coordinate]): (Coordinate, Coordinate) = {
    val xs = coordinates.map(_.x)
    val ys = coordinates.map(_.y)
    (Coordinate(xs.min, ys.min), Coordinate(xs.max, ys.max))
  }

  def manhattan(c1: Coordinate, c2: Coordinate): Int =
    Math.abs(c1.x - c2.x) + Math.abs(c1.y - c2.y)

  override def part2(coordinates: Seq[Coordinate]): Int =
    countDistanceLessThan(10000)(coordinates)

  def countDistanceLessThan(n: Int)(coordinates: Seq[Coordinate]): Int = {
    val (min, max) = defineSquare(coordinates)
    squareCoordinates(min, max).map(distanceSum(_, coordinates)).count(_ < n)
  }

  def distanceSum(c: Coordinate, coordinates: Seq[Coordinate]): Int =
    coordinates.map(manhattan(c, _)).sum
}
