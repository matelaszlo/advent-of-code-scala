package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2023Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val puzzles =
    Table(
      ("day", "part1", "part2"),
      (1,     55108,    56324),
    )

  forAll(puzzles) { (day, part1, part2) =>
    val year = 2023

    test(s"$year: Day $day") {
      val (res1, res2) = puzzleMap(year, day).solve(resource(year, day))
      assert(res1 == part1)
      assert(res2 == part2)
    }
  }
}
