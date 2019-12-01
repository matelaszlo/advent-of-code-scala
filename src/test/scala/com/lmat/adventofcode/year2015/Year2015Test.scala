package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2015Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val puzzles =
    Table(
      ("day", "part1",      "part2"),
      (1,     232,          1783),
      (2,     1586300,      3737498),
      (3,     2572,         2631),
      (4,     117946,       3938038),
      (5,     255,          55),
      (6,     569999,       17836115),
      (7,     16076,        2797),
      (8,     1350,         2085),
      (9,     141,          736),
      (10,    492982,       6989950),
      (11,    "hxbxxyzz",   "hxcaabcc"),
      (12,    119433,       68466),
      (13,    709,          668),
      (14,    2640,         1102),
      (15,    18965440,     15862900),
      (16,    103,          405),
      (17,    1638,         17),
      (18,    1061,         1006),
      (19,    518,          200),
      (20,    831600,       884520),
      (21,    91,           158),
      (22,    953,          1289),
      (23,    184,          231),
      (24,    11846773891L, 80393059L),
      (25,    2650453,      ())
    )

  forAll(puzzles) { (day, part1, part2) =>
    val year = 2015

    test(s"$year: Day $day") {
      val (res1, res2) = puzzleMap(year, day).solve(resource(year, day))
      assert(res1 == part1)
      assert(res2 == part2)
    }
  }
}
