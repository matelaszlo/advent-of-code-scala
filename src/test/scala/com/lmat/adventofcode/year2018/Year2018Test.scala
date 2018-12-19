package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2018Test extends FunSuite with TableDrivenPropertyChecks {

  val puzzles =
    Table(
      ("day", "part1",                      "part2"),
      (1,     540,                          73056),
      (2,     5658,                         "nmgyjkpruszlbaqwficavxneo"),
      (3,     103482,                       686),
      (4,     142515,                       5370),
      (5,     11754,                        4098),
      (6,     3569,                         48978),
      (7,     "CGKMUWXFAIHSYDNLJQTREOPZBV", 1046),
      (8,     37905,                        33891),
      (9,     390093L,                      3150377341L),
      (10,    "JJXZHKFP",                   10036),
      (11,    "243,34",                     "90,214,15"),
      (12,    3421,                         2550000001195L),
      (13,    "109,23",                     "137,101"),
      (14,    "5832873106",                 20273708),
      (15,    243390,                       59886)
    )

  forAll(puzzles) { (day, part1, part2) =>
    val year = 2018

    test(s"$year: Day $day") {
      val (res1, res2) = puzzleMap(year, day).solve(resource(year, day))
      assert(res1 == part1)
      assert(res2 == part2)
    }
  }
}

