package com.lmat.adventofcode.year2015

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2015.Day25._

class Day25Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val codes =
    Table(
      ("row", "column", "index", "code"),
      (1,     1,        1,       20151125L),
      (2,     1,        2,       31916031L),
      (1,     2,        3,       18749137L),
      (3,     1,        4,       16080970L),
      (2,     2,        5,       21629792L),
      (1,     3,        6,       17289845L),
    )

  test("Day25 - Index") {
    forAll(codes) { (row, column, index, _) =>
      assert(findIndex(row, column) == index)
    }
  }

  test("Day25 - Part 1") {
    forAll(codes) { (row, column, _, code) =>
      assert(part1(row, column) == code)
    }
  }
}
