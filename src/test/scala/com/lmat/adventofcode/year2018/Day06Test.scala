package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day06._
import com.lmat.adventofcode.year2018.Day06Definitions._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day06Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawCoordinates =
    """1, 1
      |1, 6
      |8, 3
      |3, 4
      |5, 5
      |8, 9""".stripMargin

  val coordinates = Seq(
    Coordinate(1, 1),
    Coordinate(1, 6),
    Coordinate(8, 3),
    Coordinate(3, 4),
    Coordinate(5, 5),
    Coordinate(8, 9))

  test("Day06 - Parse") {
    assert(rawCoordinates.split("\n").flatMap(parseCoordinate).toSeq == coordinates)
  }

  test("Day06 - Part 1") {
    assert(part1(coordinates) == 17)
  }

  test("Day06 - Part 2") {
    assert(countDistanceLessThan(32)(coordinates) == 16)
  }
}
