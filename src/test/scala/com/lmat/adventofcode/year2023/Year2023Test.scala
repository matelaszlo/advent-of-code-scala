package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2023Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val puzzles =
    Table(
      ("day", "part1", "part2"),
      (1,     55108,       56324),
      (2,     2369,        66363),
      (3,     536576,      75741499),
      (4,     22193,       5625994),
      (5,     993500720L,  4917124L),
      (6,     293046,      35150181),
      (7,     249638405L,  249776650L),
      (8,     12169,       12030780859469L),
      (9,     1708206096L, 1050),
      (10,    6882,        491)
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
