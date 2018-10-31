package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day18._
import com.lmat.adventofcode.year2015.Day18Definitions._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day18Test extends FunSuite with TableDrivenPropertyChecks {
  val rawGrid =
    """.#.#.#
      |...##.
      |#....#
      |..#...
      |#.#..#
      |####..""".stripMargin

  val grid = Grid(
    Set(
      (0, 1), (0, 3), (0, 5),
      (1, 3), (1, 4),
      (2, 0), (2, 5),
      (3, 2),
      (4, 0), (4, 2), (4, 5),
      (5, 0), (5, 1), (5, 2), (5, 3)
    ), 6)

  val grid2 = Grid(
    Set(
      (0, 2), (0, 3),
      (1, 2), (1, 3), (1, 5),
      (2, 3), (2, 4),
      (4, 0),
      (5, 0), (5, 2), (5, 3)
    ), 6)

  val grid3 = Grid(
    Set(
      (0, 2), (0, 3), (0, 4),
      (2, 2), (2, 3), (2, 4),
      (4, 1),
      (5, 1)
    ),6)

  val gridV2 = Grid(
    Set(
      (0, 0), (0, 1), (0, 3), (0, 5),
      (1, 3), (1, 4),
      (2, 0), (2, 5),
      (3, 2),
      (4, 0), (4, 2), (4, 5),
      (5, 0), (5, 1), (5, 2), (5, 3), (5, 5)
    ), 6)

  val grid2V2 = Grid(
    Set(
      (0, 0), (0, 2), (0, 3), (0, 5),
      (1, 0), (1, 1), (1, 2), (1, 3), (1, 5),
      (2, 3), (2, 4),
      (4, 0), (4, 4),
      (5, 0), (5, 2), (5, 3), (5, 4), (5, 5)
    ), 6)

  val grid3V2 = Grid(
    Set(
      (0, 0), (0, 3), (0, 5),
      (1, 0), (1, 5),
      (2, 1), (2, 3), (2, 4),
      (3, 3), (3, 4),
      (4, 1), (4, 4), (4, 5),
      (5, 0), (5, 1), (5, 3), (5, 4), (5, 5)
    ),6)

  test("Day18 - Parse") {
    assert(parseGrid(rawGrid.split("\n")) == grid)
  }

  test("Day18 - Next") {
    assert(next(isOn1)(grid)  == grid2)
    assert(next(isOn1)(grid2) == grid3)

    assert(next(isOn2)(gridV2)  == grid2V2)
    assert(next(isOn2)(grid2V2) == grid3V2)
  }
}
