package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day06Definitions._
import com.lmat.adventofcode.year2023.Day06._

class Day06Test extends AnyFunSuite {
  val raw =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin

  val parsed = List(Race(7, 9), Race(15, 40), Race(30, 200))
  val parsed2 = Race(71530, 940200)


  test("parse") {
    assert(parseRaces(raw.split("\n").toList) == parsed)
    assert(parseRace(raw.split("\n").toList) == parsed2)
  }

  test("part1") {
    assert(part1(parsed) == 288)
  }

  test("part2") {
    assert(part2(parsed2) == 71503)
  }
}
