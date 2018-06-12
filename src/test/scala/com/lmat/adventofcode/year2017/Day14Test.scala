package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day14.{preProcess, part1, part2}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day14Test extends FunSuite with TableDrivenPropertyChecks {
  val examples =
    Table(
      ("Key",      "Used", "Regions"),
      ("flqrgnkx", 8108,   1242)
    )

  test("Day14 - Part 1") {
    forAll(examples) { (key, used, _) =>
      assert(part1(preProcess(key)) == used)
    }
  }

  test("Day14 - Part 2") {
    forAll(examples) { (key, _, regions) =>
      assert(part2(preProcess(key)) == regions)
    }
  }
}
