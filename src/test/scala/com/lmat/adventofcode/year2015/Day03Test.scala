package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day03._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day03Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val directions =
    Table(
      ("directions", "simple", "with-robo"),
      (">",          2,        2),
      ("^v",         2,        3),
      ("^>v<",       4,        3),
      ("^v^v^v^v^v", 2,        11)
    )

  test("Day03 - Part 1") {
    forAll(directions) { (directions, houses, _) =>
      assert(part1(directions) == houses)
    }
  }

  test("Day03 - Part 2") {
    forAll(directions) { (directions, _, houses) =>
      assert(part2(directions) == houses)
    }
  }
}
