package com.lmat.adventofcode.year2019
import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2019Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val puzzles =
    Table(
      ("day", "part1", "part2"),
      (1,     3315133, 4969831),
      (2,     6568671, 3951),
      (3,     1211,    101386),
      (4,     1640,    1126),
      (5,     5182797, 12077198)
    )

  forAll(puzzles) { (day, part1, part2) =>
    val year = 2019

    test(s"$year: Day $day") {
      val (res1, res2) = puzzleMap(year, day).solve(resource(year, day))
      assert(res1 == part1)
      assert(res2 == part2)
    }
  }
}


