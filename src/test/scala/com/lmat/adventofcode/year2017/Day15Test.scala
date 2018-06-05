package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day15.{part1, part2}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day15Test extends FunSuite with TableDrivenPropertyChecks {
  val examples =
    Table(
      ("StartA", "StartB", "Count1", "Count2"),
      (65L,      8921L,    588L,     309L)
    )

  test("Day15 - Part 1") {
    forAll(examples) { (startA, startB, count, _) =>
      assert(part1((startA, startB)) == count)
    }
  }

  test("Day15 - Part 2") {
    forAll(examples) { (startA, startB, _, count) =>
      assert(part2((startA, startB)) == count)
    }
  }
}
