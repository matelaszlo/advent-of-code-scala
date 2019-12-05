package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.year2019.Day04._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day04Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val numbers =
    Table(
      ("number", "monotonicallyIncreasing", "hasTwoAdjacent", "hasTwoStandAloneAdjacent"),
      (111111,   true,                      true,             false),
      (223450,   false,                     true,             true),
      (123789,   true,                      false,            false),
      (112233,   true,                      true,             true),
      (123444,   true,                      true,             false),
      (111122,   true,                      true,             true),
    )

  test("Day04 - Tests") {
    forAll(numbers) { (n, m, ad, ad2) =>
      val digits = toDigits(n)
      assert(monotonicallyIncreasing(digits) == m)
      assert(hasTwoAdjacent(digits) == ad)
      assert(hasTwoStandAloneAdjacent(digits) == ad2)
    }
  }
}
