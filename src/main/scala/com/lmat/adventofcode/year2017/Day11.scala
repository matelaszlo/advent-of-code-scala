package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.Puzzle
import com.lmat.util.Files.readResource

object Day11 extends Puzzle[Seq[String], Int, Int] {
  override def parse(resource: String): Seq[String] =
    readResource(resource).head.split(",")

  case class CubeCoordinates(x: Int, y: Int, z: Int)

  /**
    * Hexagonal grids have well defined mathematics which makes this exercise fairly simple
    * 1. Find end coordinates by applying the steps
    * 2. Calculate manhattan distance from start to end coordinates
    *
    * Alternatively you can solve it by defining replace (e.g. ne, s => se) and cancel rules (e.g. n, s) to find out the minimum necessary steps taken to destination
    */
  override def part1(steps: Seq[String]): Int = {
    val start = CubeCoordinates(0, 0, 0)
    val endPosition = steps.foldLeft(start)(next)
    manhattanDistance(start, endPosition)
  }

  override def part2(steps: Seq[String]): Int = {
    val start = CubeCoordinates(0, 0, 0)
    val endPositions = steps.scanLeft(start)(next)
    endPositions.map(manhattanDistance(start, _)).max
  }

  def next(coordinates: CubeCoordinates, step: String): CubeCoordinates = step match {
    case "ne" => coordinates.copy(x = coordinates.x + 1, z = coordinates.z - 1)
    case "sw" => coordinates.copy(x = coordinates.x - 1, z = coordinates.z + 1)

    case "nw" => coordinates.copy(x = coordinates.x - 1, y = coordinates.y + 1)
    case "se" => coordinates.copy(x = coordinates.x + 1, y = coordinates.y - 1)

    case "n"  => coordinates.copy(y = coordinates.y + 1, z = coordinates.z - 1)
    case "s"  => coordinates.copy(y = coordinates.y - 1, z = coordinates.z + 1)
  }

  def manhattanDistance(a: CubeCoordinates, b: CubeCoordinates): Int =
    (Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z)) / 2
}
