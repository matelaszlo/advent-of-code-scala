package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2020Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val puzzles =
    Table(
      ("day", "part1", "part2"),
      (1,     980499,   200637446),
      (2,     483,      482),
      (7,     242,      176035)
    )

  forAll(puzzles) { (day, part1, part2) =>
    val year = 2020

    test(s"$year: Day $day") {
      val (res1, res2) = puzzleMap(year, day).solve(resource(year, day))
      assert(res1 == part1)
      assert(res2 == part2)
    }
  }
}
