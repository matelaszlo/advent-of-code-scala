package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day15._
import com.lmat.adventofcode.year2023.Day15Definitions._

class Day15Test extends AnyFunSuite {
  val raw = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
  val parsed = raw.split(",").toList
  val operations = List(Equal("rn", 1), Dash("cm"), Equal("qp", 3), Equal("cm", 2), Dash("qp"), Equal("pc", 4), Equal("ot", 9), Equal("ab", 5), Dash("pc"), Equal("pc", 6), Equal("ot", 7))

  test("parse") {
    assert(parsed.flatMap(parseOperation) == operations)
  }

  test("hash") {
    assert(parsed.map(hash) == List(30, 253, 97, 47, 14, 180, 9, 197, 48, 214, 231))
  }

  test("simulate") {
    assert(simulate(operations).filter(_._2.nonEmpty) == Map(
      0 -> Vector(("rn", 1), ("cm", 2)),
      3 -> Vector(("ot", 7), ("ab", 5), ("pc", 6))))
  }

  test("part1") {
    assert(part1(parsed) == 1320)
  }

  test("part2") {
    assert(part2(operations) == 145)
  }
}
