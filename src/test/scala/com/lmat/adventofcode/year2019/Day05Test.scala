package com.lmat.adventofcode.year2019

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2019.Day05._

class Day05Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val testPrograms =
    Table(
      ("program",                                  "input", "output"),
      // Is equal to 8 - position mode
      (Vector(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 7,       0),
      (Vector(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 8,       1),
      (Vector(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 9,       0),
      // Is equal to 8 - immediate mode
      (Vector(3, 3, 1108, -1, 8, 3, 4, 3, 99),     7,       0),
      (Vector(3, 3, 1108, -1, 8, 3, 4, 3, 99),     8,       1),
      (Vector(3, 3, 1108, -1, 8, 3, 4, 3, 99),     9,       0),
      // Is less than 8 - position mode
      (Vector(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 7,       1),
      (Vector(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 8,       0),
      (Vector(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 9,       0),
      // Is less than 8 - immediate mode
      (Vector(3, 3, 1107, -1, 8, 3, 4, 3, 99),     7,       1),
      (Vector(3, 3, 1107, -1, 8, 3, 4, 3, 99),     8,       0),
      (Vector(3, 3, 1107, -1, 8, 3, 4, 3, 99),     9,       0),
      // Jump tests - position mode
      (Vector(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9),     0,       0),
      (Vector(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9),     10,      1),
      // Jump tests - immediate mode
      (Vector(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1),             0,       0),
      (Vector(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1),             10,      1),
      // Is equal to 8 - large test
      (Vector(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 7, 999),
      (Vector(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 8, 1000),
      (Vector(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99), 9, 1001),
    )

  test("Solve programs") {
    forAll(testPrograms) { (program, input, expected) =>
      val outputs = solveProgram(program)(input)
//      println(s"Solving $program with $input produced $outputs - we expected the last output to be $expected")
      assert(outputs.last == expected)
    }
  }
}
