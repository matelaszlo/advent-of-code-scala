package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day11._

class Day11Test extends AnyFunSuite {

  val raw1 =
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....""".stripMargin

  val raw2 =
    """....#........
      |.........#...
      |#............
      |.............
      |.............
      |........#....
      |.#...........
      |............#
      |.............
      |.............
      |.........#...
      |#....#.......""".stripMargin

  val universe1 = parseUniverse(raw1.split("\n").toList)
  val expanded1 = parseUniverse(raw2.split("\n").toList)


  test("expand") {
    val rows = List(3, 7)
    val columns = List(2, 5, 8)

    assert(emptyRows(universe1) == rows)
    assert(emptyColumns(universe1) == columns)

    assert(expand(galaxies(universe1), rows, columns, 2) == galaxies(expanded1))
  }

  test("part1") {
    assert(part1(universe1) == 374)
  }

  test("galaxyDistances") {
    assert(galaxyDistances(universe1, 10) == 1030)
    assert(galaxyDistances(universe1, 100) == 8410)
  }
}
