package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day09._
class Day09Test extends AnyFunSuite {
  val raw =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin

  val oasis = List(
    List(0, 3, 6, 9, 12, 15),
    List(1, 3, 6, 10, 15, 21),
    List(10, 13, 16, 21, 30, 45)
  )

  test("parse") {
    assert(raw.split("\n").toList.map(parseRow) == oasis)
  }

  test("part1") {
    assert(part1(oasis) == 114)
  }

  test("part2") {
    assert(part2(oasis) == 2)
  }
}
