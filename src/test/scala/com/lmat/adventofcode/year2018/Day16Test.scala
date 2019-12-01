package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day16._
import com.lmat.adventofcode.year2018.Day16Definitions._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day16Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val instructions =
    Table(
      ("before", "instruction", "after"),
      (Seq(3, 2, 1, 1), Instruction("addr", 2, 1, 2), Seq(3, 2, 3, 1)),
      (Seq(3, 2, 1, 1), Instruction("addi", 2, 1, 2), Seq(3, 2, 2, 1)),
      (Seq(3, 2, 1, 1), Instruction("mulr", 2, 1, 2), Seq(3, 2, 2, 1)),
      (Seq(3, 2, 1, 1), Instruction("muli", 2, 1, 2), Seq(3, 2, 1, 1)),

      (Seq(3, 2, 1, 1), Instruction("banr", 2, 1, 2), Seq(3, 2, 0, 1)),
      (Seq(3, 2, 1, 1), Instruction("bani", 2, 1, 2), Seq(3, 2, 1, 1)),
      (Seq(3, 2, 1, 1), Instruction("borr", 2, 1, 2), Seq(3, 2, 3, 1)),
      (Seq(3, 2, 1, 1), Instruction("bori", 2, 1, 2), Seq(3, 2, 1, 1)),

      (Seq(3, 2, 1, 1), Instruction("setr", 2, 1, 2), Seq(3, 2, 1, 1)),
      (Seq(3, 2, 1, 1), Instruction("seti", 2, 1, 2), Seq(3, 2, 2, 1)),

      (Seq(3, 2, 1, 1), Instruction("gtir", 2, 1, 2), Seq(3, 2, 0, 1)),
      (Seq(3, 2, 1, 1), Instruction("gtri", 2, 1, 2), Seq(3, 2, 0, 1)),
      (Seq(3, 2, 1, 1), Instruction("gtrr", 2, 1, 2), Seq(3, 2, 0, 1)),

      (Seq(3, 2, 1, 1), Instruction("eqir", 2, 1, 2), Seq(3, 2, 1, 1)),
      (Seq(3, 2, 1, 1), Instruction("eqri", 2, 1, 2), Seq(3, 2, 1, 1)),
      (Seq(3, 2, 1, 1), Instruction("eqrr", 2, 1, 2), Seq(3, 2, 0, 1)),
    )

  test("Day16 - Instructions") {
    forAll(instructions) { (before, instruction, after) =>
      assert(applyInstruction(instruction, before) == after)
    }
  }

  test("Day16 - Matching Instructions") {
    assert(matchingInstructions(instructionNames)(CpuSample(Seq(3, 2, 1, 1), InstructionTemplate(9, 2, 1, 2), Seq(3, 2, 2, 1))) == Set(Instruction("addi", 2, 1, 2), Instruction("mulr", 2, 1, 2), Instruction("seti", 2, 1, 2)))
  }
}
