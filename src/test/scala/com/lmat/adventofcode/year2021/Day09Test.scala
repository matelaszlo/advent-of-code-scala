package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.year2021.Day09._
import org.scalatest.funsuite.AnyFunSuite

class Day09Test extends AnyFunSuite {
  val raw =
    """2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678""".stripMargin

  val heightmap = parseHeightMap(raw.split("\n"))

  test("lowPoints") {
    assert(lowPoints(heightmap) == Set((1, 0, 1), (9, 0, 0), (2, 2, 5), (6, 4, 5)))
  }

  test("basins") {
    assert(basins(heightmap) == Set(
      Set((3, 1, 7), (3, 3, 7), (1, 2, 8), (4, 1, 8), (2, 3, 6), (1, 4, 8), (4, 2, 7), (4, 3, 8), (2, 1, 8), (3, 2, 6), (2, 2, 5), (1, 3, 7), (0, 3, 8), (5, 2, 8)),
      Set((9, 4, 8), (7, 2, 8), (8, 4, 7), (6, 4, 5), (7, 3, 7), (5, 4, 6), (7, 4, 6), (6, 3, 6), (8, 3, 8)),
      Set((8, 1, 2), (7, 0, 2), (9, 2, 2), (9, 0, 0), (8, 0, 1), (6, 1, 4), (6, 0, 3), (9, 1, 1), (5, 0, 4)),
      Set((1, 0, 1), (0, 0, 2), (0, 1, 3))
    ))
  }

  test("part1") {
    assert(part1(heightmap) == 15)
  }

  test("part2") {
    assert(part2(heightmap) == 1134)
  }
}
