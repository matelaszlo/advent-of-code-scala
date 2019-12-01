package com.lmat.adventofcode.year2018

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2018.Day02._

class Day02Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val boxIDs = Seq(
    "abcdef",
    "bababc",
    "abbcde",
    "abcccd",
    "aabcdd",
    "abcdee",
    "ababab"
  )

  test("Day02 - Part 1") {
    assert(part1(boxIDs) == 12)
  }

  val boxIDs2 = Seq(
    "abcde",
    "fghij",
    "klmno",
    "pqrst",
    "fguij",
    "axcye",
    "wvxyz"
  )

  test("Day02 - Part 2") {
    assert(part2(boxIDs2) == "fgij")
  }
}
