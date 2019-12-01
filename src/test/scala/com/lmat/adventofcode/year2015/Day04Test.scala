package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day04.part1
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day04Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val directions =
    Table(
      ("key",     "number"),
      ("abcdef",  609043),
      ("pqrstuv", 1048970),
    )

  test("Day04 - Part 1") {
    forAll(directions) { (key, number) =>
      assert(part1(key) == number)
    }
  }

}
