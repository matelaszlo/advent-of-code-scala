package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day07Definitions._
import com.lmat.adventofcode.year2015.Day07._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day07Test extends FunSuite with TableDrivenPropertyChecks {

  val rawInstructions =
    """123 -> x
      |456 -> y
      |x AND y -> d
      |x OR y -> e
      |x LSHIFT 2 -> f
      |y RSHIFT 2 -> g
      |NOT x -> h
      |NOT y -> i
    """.stripMargin

  val instructions = Seq(
    Instruction(Identity("123"), "x"),
    Instruction(Identity("456"), "y"),
    Instruction(And("x", "y"), "d"),
    Instruction(Or("x", "y"), "e"),
    Instruction(LShift("x", 2), "f"),
    Instruction(RShift("y", 2), "g"),
    Instruction(Not("x"), "h"),
    Instruction(Not("y"), "i"))

  test("Day07 - Parse") {
    assert(rawInstructions.split("\n").toSeq.flatMap(parseInstruction) == instructions)
  }

  test("Day07 - Emulate") {
    val expected = Map(
      "d" -> 72,
      "e" -> 507,
      "f" -> 492,
      "g" -> 114,
      "h" -> 65412,
      "i" -> 65079,
      "x" -> 123,
      "y" -> 456)

    assert(emulate(instructions) == expected)
  }
}
