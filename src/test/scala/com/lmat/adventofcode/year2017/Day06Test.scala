package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day06.{part1, part2, preProcess, redistribute}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day06Test extends FunSuite with TableDrivenPropertyChecks {
  val examples =
    Table(
      ("Blocks",        "Count", "Size"),
      (Vector(0,2,7,0), 5,       4)
    )

  test("Day06 - Part 1") {
    forAll(examples) { (blocks, steps, _) =>
      assert(part1(preProcess(blocks)) == steps)
    }
  }

  test("Day06 - Part 2") {
    forAll(examples) { (blocks, _, size) =>
      assert(part2(preProcess(blocks)) == size)
    }
  }

  val redistributeData =
    Table(
      ("Input",            "result"),
      (Vector(0, 2, 7, 0), Vector(2, 4, 1, 2)),
      (Vector(2, 4, 1, 2), Vector(3, 1, 2, 3)),
      (Vector(3, 1, 2, 3), Vector(0, 2, 3, 4)),
      (Vector(0, 2, 3, 4), Vector(1, 3, 4, 1)),
      (Vector(1, 3, 4, 1), Vector(2, 4, 1, 2)),
      (Vector(0, 0, 2, 0), Vector(1, 0, 0, 1))
    )

  test("Redistribute") {
    forAll(redistributeData) { (input, result) =>
      assert(redistribute(input) == result)
    }
  }
}
