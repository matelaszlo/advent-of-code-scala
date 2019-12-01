package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day13.{parseLayers, part1, part2}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day13Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val input =
    """0: 3
      |1: 2
      |4: 4
      |6: 4""".stripMargin

  val layers = Map(
    0 -> 3,
    1 -> 2,
    4 -> 4,
    6 -> 4
  )

  test("Parse input") {
    assert(parseLayers(input.split("\n").toIndexedSeq) == layers)
  }

  val examples =
    Table(
      ("Layers", "Severity", "Delay"),
      (layers,    24,         10)
    )

  test("Day13 - Part 1") {
    forAll(examples) { (layers, severity, _) =>
      assert(part1(layers) == severity)
    }
  }

  test("Day13 - Part 2") {
    forAll(examples) { (layers, _, delay) =>
      assert(part2(layers) == delay)
    }
  }
}
