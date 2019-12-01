package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day08Definitions._
import com.lmat.adventofcode.year2017.Day08.{parseInstruction, part1, part2}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day08Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val rawInstructions =
    """b inc 5 if a > 1
      |a inc 1 if b < 5
      |c dec -10 if a >= 1
      |c inc -20 if c == 10""".stripMargin

  val instructions = Seq(
    Instruction("b", "inc", 5, Condition("a", ">", 1)),
    Instruction("a", "inc", 1, Condition("b", "<", 5)),
    Instruction("c", "dec", -10, Condition("a", ">=", 1)),
    Instruction("c", "inc", -20, Condition("c", "==", 10))
  )

  test("Parse Instructions") {
    assert(rawInstructions.split("\n").toSeq.map(parseInstruction) == instructions)
  }

  test("Day08 - Part 1") {
    assert(part1(instructions) == 1)
  }

  test("Day08 - Part 2") {
    assert(part2(instructions) == 10)
  }
}
