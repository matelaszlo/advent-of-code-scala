package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day05._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day05Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val polymers =
    Table(
      ("polymer",          "reacted",    "part1"),
      ("aA",               "",           0),
      ("abBA",             "",           0),
      ("abAB",             "abAB",       4),
      ("aabAAB",           "aabAAB",     6),
      ("dabAcCaCBAcCcaDA", "dabCBAcaDA", 10),
    )

  test("Day05 - Reaction") {
    forAll(polymers) { (polymer, reacted, _) =>
      assert(react(polymer) == reacted)
    }
  }

  test("Day05 - Part 1") {
    forAll(polymers) { (polymer, _, result) =>
      assert(part1(preProcess(polymer)) == result)
    }
  }

  test("Day05 - Part 2") {
    assert(part2(preProcess("dabAcCaCBAcCcaDA")) == 4)
  }
}
