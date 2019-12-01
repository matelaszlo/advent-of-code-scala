package com.lmat.adventofcode.year2015

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

import com.lmat.adventofcode.year2015.Day23Definitions._
import com.lmat.adventofcode.year2015.Day23._

class Day23Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val rawInstructions =
    """inc a
      |jio a, +2
      |tpl a
      |inc a""".stripMargin

  val instructions = Seq(
    Increment("a"),
    JumpIfOdd("a",2),
    Triple("a"),
    Increment("a")
  )

  test("Day23 - Parse") {
    assert(rawInstructions.split("\n").flatMap(parseInstruction).toSeq == instructions)
  }

  test("Day23 - Apply") {
    assert(applyInstructions(State(Map("a" -> 0, "b" -> 0), 0), instructions) == State(Map("a" -> 2, "b" -> 0),4))
  }
}
