package com.lmat.adventofcode.year2020

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2020.Day01._

class Day01Test extends AnyFunSuite {
  val expenses = Array(1721, 979, 366, 299, 675, 1456)

  test("sumsUpTo") {
    assert(sumsUpTo(expenses.sorted)(2020).get == (299, 1721))
    assert(sumsThreeUpTo(expenses.sorted)(2020).get == (366, 675, 979))
  }

  test("part1") {
    assert(part1(expenses) == 514579)
  }

  test("part2") {
    assert(part2(expenses) == 241861950)
  }
}
