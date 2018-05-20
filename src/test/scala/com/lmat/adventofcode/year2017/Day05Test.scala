package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day05.{part1, part2}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day05Test extends FunSuite with TableDrivenPropertyChecks {

  val offsets =
    Table(
      ("offsets",              "steps"),
      (Vector(0, 3, 0, 1, -3), 5)
    )

  test("Day02 - Part 1") {
    forAll(offsets) { (offsets, steps) =>
      assert(part1(offsets) == steps)
    }
  }

  val offsets2 =
    Table(
      ("offsets",              "steps"),
      (Vector(0, 3, 0, 1, -3), 10)
    )

  test("Day02 - Part 2") {
    forAll(offsets2) { (offsets, steps) =>
      assert(part2(offsets) == steps)
    }
  }
}
