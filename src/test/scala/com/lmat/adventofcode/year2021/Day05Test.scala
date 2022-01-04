package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.year2021.Day05Definitions._
import com.lmat.adventofcode.year2021.Day05._
import org.scalatest.funsuite.AnyFunSuite

class Day05Test extends AnyFunSuite {
  val raw =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin

  val lines =
    List(
      Line(Coordinate(0, 9), Coordinate(5, 9)),
      Line(Coordinate(8, 0), Coordinate(0, 8)),
      Line(Coordinate(9, 4), Coordinate(3, 4)),
      Line(Coordinate(2, 2), Coordinate(2, 1)),
      Line(Coordinate(7, 0), Coordinate(7, 4)),
      Line(Coordinate(6, 4), Coordinate(2, 0)),
      Line(Coordinate(0, 9), Coordinate(2, 9)),
      Line(Coordinate(3, 4), Coordinate(1, 4)),
      Line(Coordinate(0, 0), Coordinate(8, 8)),
      Line(Coordinate(5, 5), Coordinate(8, 2))
    )

  test("parseLines") {
    assert(raw.split("\n").flatMap(parseLine).toList == lines)
  }

  test("part1") {
    assert(part1(lines) == 5)
  }

  test("part2") {
    assert(part2(lines) == 12)
  }
}
