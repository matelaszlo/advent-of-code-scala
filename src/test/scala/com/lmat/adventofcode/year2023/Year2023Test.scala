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
      (10,    6882,        491),
      (11,    9312968,     597714117556L),
      (12,    6871,        2043098029844L),
      (13,    37718,       40995),
      (14,    106990,      100531),
      (15,    516070,      244981),
      (16,    8901,        9064),
      (17,    928,         1104),
//      (18,    35991,       54058824661845L), Needs cleanup/optimization to commit
      (19,    330820,       123972546935551L),
      (20,    919383692,    247702167614647L),
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
