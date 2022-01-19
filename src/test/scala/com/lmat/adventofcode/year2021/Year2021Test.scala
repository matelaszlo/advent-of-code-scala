package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2021Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val puzzles =
    Table(
      ("day", "part1", "part2"),
      (1,     1292,    1262),
      (2,     1947824, 1813062561),
      (3,     2035764, 2817661),
      (4,     64084,   12833),
      (5,     4873,    19472),
      (6,     380758,  1710623015163L),
      (7,     351901,  101079875),
      (8,     514,     1012272),
      (9,     566,     891684),
    )

  forAll(puzzles) { (day, part1, part2) =>
    val year = 2021

    test(s"$year: Day $day") {
      val (res1, res2) = puzzleMap(year, day).solve(resource(year, day))
      assert(res1 == part1)
      assert(res2 == part2)
    }
  }
}
