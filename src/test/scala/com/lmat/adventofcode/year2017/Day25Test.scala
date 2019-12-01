package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day25._
import com.lmat.adventofcode.year2017.Day25Definitions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day25Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val rawConfig =
    """Begin in state A.
      |Perform a diagnostic checksum after 6 steps.
      |
      |In state A:
      |  If the current value is 0:
      |    - Write the value 1.
      |    - Move one slot to the right.
      |    - Continue with state B.
      |  If the current value is 1:
      |    - Write the value 0.
      |    - Move one slot to the left.
      |    - Continue with state B.
      |
      |In state B:
      |  If the current value is 0:
      |    - Write the value 1.
      |    - Move one slot to the left.
      |    - Continue with state A.
      |  If the current value is 1:
      |    - Write the value 1.
      |    - Move one slot to the right.
      |    - Continue with state A.""".stripMargin

  val config = TuringMachineConfig("A", 6, Map(
    "A" -> State("A", Instruction(1, "right", "B"), Instruction(0, "left",  "B")),
    "B" -> State("B", Instruction(1, "left", "A"),  Instruction(1, "right", "A")))
  )

  test("Parse") {
    assert(parseInstructions(rawConfig.split("\n").toIndexedSeq) == config)
  }

  test("Day 25 - Part 1") {
    assert(part1(config) == 3)
  }
}
