package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day05.{nice, nice2}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day05Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val strings =
    Table(
      ("string",           "nice"),
      ("ugknbfddgicrmopn", true),
      ("aaa",              true),
      ("jchzalrnumimnmhp", false),
      ("haegwjzuvuyypxyu", false),
      ("dvszwmarrgswjxmb", false)
    )

  test("Day05 - Part 1") {
    forAll(strings) { (string, isNice) =>
      assert(nice(string) == isNice)
    }
  }

  val strings2 =
    Table(
      ("string",           "nice"),
      ("qjhvhtzxzqqjkmpb", true),
      ("xxyxx",            true),
      ("uurcxstgmygtbstg", false),
      ("ieodomkazucvgmuy", false)
    )

  test("Day05 - Part 2") {
    forAll(strings2) { (string, isNice) =>
      assert(nice2(string) == isNice)
    }
  }
}
