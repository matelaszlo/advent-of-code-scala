package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day11._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day11Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val cells =
    Table(
      ("x", "y", "serial number", "power"),
      (3,   5,   8,               4),
      (122, 79,  57,              -5),
      (217, 196, 39,              0),
      (101, 153, 71,              4),
    )

  test("Day11 - Calculate Power Level") {
    forAll(cells) { (x, y, serial, power) =>
      assert(calculatePowerLevel(serial)(x, y) == power)
    }
  }

  test("Day11 - Part 1") {
    assert(part1(42) == "21,61")
  }

  test("Day11 - Part 2") {
    assert(part2(18) == "90,269,16")
    assert(part2(42) == "232,251,12")
  }
}
