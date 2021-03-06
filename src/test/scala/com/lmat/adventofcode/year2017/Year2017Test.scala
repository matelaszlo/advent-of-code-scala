package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2017Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val puzzles =
    Table(
      ("day", "part1",            "part2"),
      (1,     1390,               1232),
      (2,     37923,              263),
      (3,     480,                349975),
      (4,     325,                119),
      (5,     339351,             24315397),
      (6,     12841,              8038),
      (7,     "airlri",           1206),
      (8,     6611,               6619),
      (9,     11089,              5288),
      (10,    11413,              "7adfd64c2a03a4968cf708d1b7fd418d"),
      (11,    698,                1435),
      (12,    283,                195),
      (13,    1624,               3923436),
      (14,    8204,               1089),
      (15,    609,                253),
      (16,    "doeaimlbnpjchfkg", "agndefjhibklmocp"),
      (17,    1244,               11162912),
      (18,    9423,               7620),
      (19,    "SXPZDFJNRL",       18126),
      (20,    308,                504),
      (21,    162,                2264586),
      (22,    5411,               2511416),
      (23,    9409,               913),
      (24,    1511,               1471),
      (25,    4217,               ())
    )

  forAll(puzzles) { (day, part1, part2) =>
    val year = 2017

    test(s"$year: Day $day") {
      val (res1, res2) = puzzleMap(year, day).solve(resource(year, day))
      assert(res1 == part1)
      assert(res2 == part2)
    }
  }
}
