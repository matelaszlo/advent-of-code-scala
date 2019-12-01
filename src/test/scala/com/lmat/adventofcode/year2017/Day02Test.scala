package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day02.{part1, part2, parseSpreadsheet}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day02Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val raws =
    Table(
      ("raw",                     "spreadsheet"),
      ("5 1 9 5\n7 5 3\n2 4 6 8",   Seq(Seq(5, 1, 9, 5), Seq(7, 5, 3),    Seq(2, 4, 6, 8))),
      ("5 9 2 8\n9 4 7 3\n3 8 6 5", Seq(Seq(5, 9, 2, 8), Seq(9, 4, 7, 3), Seq(3, 8, 6, 5)))
    )

  test("Parse") {
    forAll(raws) { (raw, spreadsheet) =>
      assert(parseSpreadsheet(raw.split("\n").toIndexedSeq) == spreadsheet)
    }
  }

  val spreadsheets =
    Table(
      ("spreadsheet",                                       "checksum"),
      (Seq(Seq(5, 1, 9, 5), Seq(7, 5, 3), Seq(2, 4, 6, 8)), 18)
    )

  test("Day02 - Part 1") {
    forAll(spreadsheets) { (spreadsheet, check) =>
      assert(part1(spreadsheet) == check)
    }
  }

  val spreadsheets2 =
    Table(
      ("spreadsheet",                                          "checksum"),
      (Seq(Seq(5, 9, 2, 8), Seq(9, 4, 7, 3), Seq(3, 8, 6, 5)), 9)
    )

  test("Day02 - Part 2") {
    forAll(spreadsheets2) { (spreadsheet, check) =>
      assert(part2(spreadsheet) == check)
    }
  }
}
