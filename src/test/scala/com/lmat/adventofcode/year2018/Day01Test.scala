package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day01._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day01Test extends FunSuite with TableDrivenPropertyChecks {
  val frequencies =
    Table(
      ("frequencies",   "result"),
      (Seq(+1, +1, +1),  3),
      (Seq(+1, +1, -2),  0),
      (Seq(-1, -2, -3), -6),
    )

  test("Day01 - Part 1") {
    forAll(frequencies) { (frequencies, result) =>
      assert(part1(frequencies) == result)
    }
  }

  val frequencies2 =
    Table(
      ("frequencies",           "result"),
      (Seq(+1, -1),             0),
      (Seq(+3, +3, +4, -2, -4), 10),
      (Seq(-6, +3, +8, +5, -6), 5),
      (Seq(+7, +7, -2, -7, -4), 14),
    )

  test("Day01 - Part 2") {
    forAll(frequencies2) { (frequencies, result) =>
      assert(part2(frequencies) == result)
    }
  }
}
