package com.lmat.adventofcode.year2020

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2020.Day02._
import com.lmat.adventofcode.year2020.Day02Definitions._

class Day02Test extends AnyFunSuite {
  val passwordsRaw =
    """1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc""".stripMargin

  val passwords = List(
    (Policy('a', 1, 3), "abcde"),
    (Policy('b', 1, 3), "cdefg"),
    (Policy('c', 2, 9), "ccccccccc")
  )

  test("parse") {
    assert(passwordsRaw.split("\n").flatMap(parsePolicyRow).toList == passwords)
  }

  test("part1") {
    assert(part1(passwords) == 2)
  }

  test("part2") {
    assert(part2(passwords) == 1)
  }
}
