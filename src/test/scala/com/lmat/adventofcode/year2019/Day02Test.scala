package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.year2019.Day02._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day02Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val intCodes =
    Table(
      ("source",                    "result"),
      (Vector(1,0,0,0,99),          Vector(2,0,0,0,99)),
      (Vector(2,3,0,3,99),          Vector(2,3,0,6,99)),
      (Vector(2,4,4,5,99,0),        Vector(2,4,4,5,99,9801)),
      (Vector(1,1,1,4,99,5,6,0,99), Vector(30,1,1,4,2,5,6,0,99)),
    )

  test("Day02 - Solve") {
    forAll(intCodes) { (source, result) =>
      assert(solve(source) == result)
    }
  }
}
