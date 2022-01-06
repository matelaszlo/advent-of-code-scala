package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.year2021.Day06._
import org.scalatest.funsuite.AnyFunSuite

class Day06Test extends AnyFunSuite {
  test("simulate") {
    assert(simulateAt(7, 2)(List(3, 4, 3, 1, 2), 1) ==
      Map(0 -> 1, 1 -> 1, 2 -> 2, 3 -> 1))
    assert(simulateAt(7, 2)(List(3, 4, 3, 1, 2), 18) ==
      Map(0 -> 3, 1 -> 5, 2 -> 3, 3 -> 2, 4 -> 2, 5 -> 1, 6 -> 5, 7 -> 1, 8 -> 4))
    assert(simulateAt(7, 2)(List(3, 4, 3, 1, 2), 80) ==
      Map(0 -> 424, 1 -> 729, 2 -> 558, 3 -> 790, 4 -> 739, 5 -> 762, 6 -> 991, 7 -> 370, 8 -> 571))
    assert(simulateAt(7, 2)(List(3, 4, 3, 1, 2), 256) ==
      Map(0 -> 2376852196L, 1 -> 2731163883L, 2 -> 2897294544L, 3 -> 3164316379L, 4 -> 3541830408L, 5 -> 3681986557L, 6 -> 4275812629L, 7 -> 1985489551L, 8 -> 2329711392L))
  }

  test("part1") {
    assert(part1(List(3, 4, 3, 1, 2)) == 5934)
  }

  test("part2") {
    assert(part2(List(3, 4, 3, 1, 2)) == 26984457539L)
  }
}
