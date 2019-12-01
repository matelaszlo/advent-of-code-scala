package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day07._
import com.lmat.adventofcode.year2018.Day07Definitions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day07Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawInstructions =
    """Step C must be finished before step A can begin.
      |Step C must be finished before step F can begin.
      |Step A must be finished before step B can begin.
      |Step A must be finished before step D can begin.
      |Step B must be finished before step E can begin.
      |Step D must be finished before step E can begin.
      |Step F must be finished before step E can begin.""".stripMargin

  val instructions = Seq(
    Instruction('C', 'A'),
    Instruction('C', 'F'),
    Instruction('A', 'B'),
    Instruction('A', 'D'),
    Instruction('B', 'E'),
    Instruction('D', 'E'),
    Instruction('F', 'E'))

  test("Day07 - Parse") {
    assert(rawInstructions.split("\n").flatMap(parseInstruction).toSeq == instructions)
  }

  test("Day07 - Part 1") {
    assert(part1(instructions) == "CABDFE")
  }

  test("Day07 - Time Map") {
    val timeM = timeMap(0)
    assert(timeM('A') == 1)
    assert(timeM('B') == 2)
    assert(timeM('Z') == 26)
  }

  test("Day07 - Part 2") {
    assert(completionTime(0, 2)(instructions) == 15)
  }
}
