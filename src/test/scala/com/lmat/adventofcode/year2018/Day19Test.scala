package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day16Definitions.Instruction
import com.lmat.adventofcode.year2018.Day19Definitions._
import com.lmat.adventofcode.year2018.Day19._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day19Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawProgram: String =
    """#ip 0
      |seti 5 0 1
      |seti 6 0 2
      |addi 0 1 0
      |addr 1 2 3
      |setr 1 0 0
      |seti 8 0 4
      |seti 9 0 5""".stripMargin

  val program = Program(0, List(
      Instruction("seti", 5, 0, 1),
      Instruction("seti", 6, 0, 2),
      Instruction("addi", 0, 1, 0),
      Instruction("addr", 1, 2, 3),
      Instruction("setr", 1, 0, 0),
      Instruction("seti", 8, 0, 4),
      Instruction("seti", 9, 0, 5)))

  test("Parse"){
    assert(parseProgram(rawProgram.split("\n").toIndexedSeq).get == program)
  }

  test("Part 1"){
    assert(part1(program) == 7)
  }
}
