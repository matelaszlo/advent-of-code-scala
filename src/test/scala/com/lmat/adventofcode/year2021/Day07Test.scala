package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.year2021.Day07._
import org.scalatest.funsuite.AnyFunSuite

class Day07Test extends AnyFunSuite {
  test("align") {
    assert(align(List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14), identity) == (2, 37))
    assert(align(List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14), steps => (1 to steps).sum) == (5, 168))
    assert(align(List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14), n => n * (n + 1) / 2) == (5, 168))
  }

  test("part1") {
    assert(part1(List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)) == 37)
  }

  test("part2") {
    assert(part2(List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)) == 168)
  }
}
