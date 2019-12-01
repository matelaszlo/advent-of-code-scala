package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day19._
import com.lmat.adventofcode.year2015.Day19Definitions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day19Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val rawGenetics =
    """H => HO
      |H => OH
      |O => HH
      |
      |HOH""".stripMargin

  val genetics  = Genetics(
    "HOH",
    Set(
      Rule("H", "HO"),
      Rule("H", "OH"),
      Rule("O", "HH")))

  test("Day19 - Parse") {
    assert(parseGenetics(rawGenetics.split("\n").toSeq) == genetics)
  }

  test("Day19 - Next Molecules") {
    assert(nextMolecules(genetics.rules)(genetics.molecule) == Set("HOOH", "HOHO", "OHOH", "HHHH"))
  }

  test("Day19 - Part 1") {
    assert(part1(genetics) == 4)
  }

  val genetics2 =
    Genetics(
      "HOH",
      Set(
        Rule("e", "H"),
        Rule("e", "O"),
        Rule("H", "HO"),
        Rule("H", "OH"),
        Rule("O", "HH")))

  test("Day19 - Part 2") {
    assert(part2(genetics2) == 3)
    assert(part2(genetics2.copy(molecule = "HOHOHO")) == 6)
  }
}
