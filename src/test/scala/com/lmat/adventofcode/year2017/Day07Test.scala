package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day07._
import com.lmat.adventofcode.year2017.Day07Definitions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day07Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val towers =
    Table(
      ("Row",                             "Tower Definition"),
      ("pbga (66)",                       TowerDefinition("pbga", 66, Set())),
      ("hubvoh (152)",                    TowerDefinition("hubvoh", 152, Set())),
      ("fwft (72) -> ktlj, cntj, xhth",   TowerDefinition("fwft", 72, Set("ktlj", "cntj", "xhth"))),
      ("vpryah (310) -> iedlpkf, epeain", TowerDefinition("vpryah", 310, Set("iedlpkf", "epeain")))
    )

  test("Parsing Tower Definitions") {
    forAll(towers) { (row, definition) =>
      assert(parseTowerDefinition(row).contains(definition))
    }
  }

  val testTowerDefinitions = Set(
    TowerDefinition("pbga", 66, Set()),
    TowerDefinition("xhth", 57, Set()),
    TowerDefinition("ebii", 61, Set()),
    TowerDefinition("havc", 66, Set()),
    TowerDefinition("ktlj", 57, Set()),
    TowerDefinition("fwft", 72, Set("ktlj", "cntj", "xhth")),
    TowerDefinition("qoyq", 66, Set()),
    TowerDefinition("padx", 45, Set("pbga", "havc", "qoyq")),
    TowerDefinition("tknk", 41, Set("ugml", "padx", "fwft")),
    TowerDefinition("jptl", 61, Set()),
    TowerDefinition("ugml", 68, Set("gyxo", "ebii", "jptl")),
    TowerDefinition("gyxo", 61, Set()),
    TowerDefinition("cntj", 57, Set())
  )

  test("Day07 - Part 1") {
    assert(part1(testTowerDefinitions) == "tknk")
  }

  val testTower =
    Tower("tknk", Set(
      Tower("fwft", Set(
        Tower("xhth", 57),
        Tower("cntj", 57),
        Tower("ktlj", 57)), 72),
      Tower("padx", Set(
        Tower("qoyq", 66),
        Tower("havc", 66),
        Tower("pbga", 66)), 45),
      Tower("ugml", Set(
        Tower("jptl", 61),
        Tower("ebii", 61),
        Tower("gyxo", 61)), 68)), 41)

  test("Build Tower") {
    assert(buildTower(testTowerDefinitions, "tknk") == testTower)
  }

  test("Get Corrected Weight") {
    assert(getCorrectedWeight(testTower) == 60)
  }

  test("Day07 - Part 2") {
    assert(part2(testTowerDefinitions) == 60)
  }
}
