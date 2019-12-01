package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day03.{part1, part2}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day03Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val examples =
    Table(
      ("input", "steps"),
      (1,       0),
      (12,      3),
      (23,      2),
      (1024,    31),
    )

  test("Day03 - Part 1") {
    forAll(examples) { (input, steps) =>
      assert(part1(input) == steps)
    }
  }

  val examples2 =
    Table(
      ("input", "value"),
      (0,       1),
      (1,       2),
      (23,      25),
      (24,      25),
    )

  test("Day03 - Part 2") {
    forAll(examples2) { (input, value) =>
      assert(part2(input) == value)
    }
  }
}
