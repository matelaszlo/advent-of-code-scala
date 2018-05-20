package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2017Test extends FunSuite with TableDrivenPropertyChecks {

  val puzzles =
    Table(
      ("day", "part1", "part2"),
      (1,     1390,    1232),
      (2,     37923,   263),
      (3,     480,     349975),
      (4,     325,     119),
      (5,     339351,  24315397)
    )

  forAll(puzzles) { (day, part1, part2) =>
    val year = 2017

    test(s"$year: Day $day - Part 1") {
      val (res1, res2) = puzzleMap(year, day).solve(resource(year, day))
      assert(res1 == part1)
      assert(res2 == part2)
    }
  }

}
