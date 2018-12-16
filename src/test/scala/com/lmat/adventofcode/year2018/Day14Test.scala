package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day14._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day14Test extends FunSuite with TableDrivenPropertyChecks {
  val recipes =
    Table(
      ("number", "score"),
      (5,        "0124515891"),
      (18,       "9251071085"),
      (2018,     "5941429882")
    )

  test("Day14 - Part 1") {
    forAll(recipes) { (number, score) =>
      assert(part1(number) == score)
    }
  }

  val recipes2 =
    Table(
      ("number", "index"),
      (51589,    9),
      (92510,    18),
      (59414,    2018)
    )

  test("Day14 - Part 2") {
    forAll(recipes2) { (number, index) =>
      assert(part2(number) == index)
    }
  }
}
