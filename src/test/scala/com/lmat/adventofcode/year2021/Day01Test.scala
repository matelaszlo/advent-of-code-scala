package com.lmat.adventofcode.year2021

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2021.Day01._

class Day01Test extends AnyFunSuite {
  val measurements = Seq(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  test("part1") {
    assert(part1(measurements) == 7)
  }

  test("part2") {
    assert(part2(measurements) == 5)
  }
}
