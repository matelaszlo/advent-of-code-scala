package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day17._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day17Test extends FunSuite with TableDrivenPropertyChecks {

  val containers = List(20, 15, 10, 5, 5)

  val combinations25 = List(
    List(20, 5),
    List(20, 5),
    List(15, 10),
    List(15, 5, 5)
  )

  test("Day17 - Combinations") {
    assert(combinations(containers, 25) == combinations25)
  }

  test("Day17 - Part 1") {
    assert(part1(combinations25) == 4)
  }

  test("Day17 - Part 2") {
    assert(part2(combinations25) == 3)
  }
}
