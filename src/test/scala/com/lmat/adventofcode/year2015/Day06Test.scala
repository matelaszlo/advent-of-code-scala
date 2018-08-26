package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day06._
import com.lmat.adventofcode.year2015.Day06Definitions._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day06Test extends FunSuite with TableDrivenPropertyChecks {
  val instructions = Seq(
    Instruction("turn on",  Point(0, 0),     Point(999, 999)),
    Instruction("toggle",   Point(0, 0),     Point(999, 0)),
    Instruction("turn off", Point(499, 499), Point(500, 500)))

  test("Day06 - Part 1") {
    assert(part1(instructions) == 998996)
  }

  val instructions2 = Seq(
    Instruction("turn on",  Point(0, 0),     Point(0, 0)),
    Instruction("toggle",   Point(0, 0),     Point(999, 999)))

  test("Day06 - Part 2") {
    assert(part2(instructions2) == 2000001)  }
}
