package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.year2019.Day01._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day01Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val masses =
    Table(
      ("mass", "result", "result2"),
      (12,     2,        2),
      (14,     2,        2),
      (1969,   654,      966),
      (100756, 33583,    50346)
    )

  test("Day01 - Part 1") {
    forAll(masses) { (mass, result, _) =>
      assert(fuel(mass) == result)
    }
  }

  test("Day01 - Part 2") {
    forAll(masses) { (mass, _, result) =>
      assert(fuel2(mass) == result)
    }
  }
}
