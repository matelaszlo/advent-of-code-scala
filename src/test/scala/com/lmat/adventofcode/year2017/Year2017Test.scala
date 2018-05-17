package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2017Test extends FunSuite with TableDrivenPropertyChecks {

  val puzzles =
    Table(
      ("day", "part1", "part2"),
      (1,     1390,    1232),
      (2,     37923,   263)
    )

  forAll(puzzles) { (day, part1, part2) =>
    val year = 2017
    val (res1, res2) = puzzleMap(year, day).solve(resource(year, day))

    test(s"$year: Day $day - Part 1") {
      assert(res1 == part1)
    }

    test(s"$year: Day $day - Part 2") {
      assert(res2 == part2)
    }
  }

}
