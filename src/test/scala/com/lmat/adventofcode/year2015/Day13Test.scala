package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day13._
import com.lmat.adventofcode.year2015.Day13Definitions.HappinessChange
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day13Test extends FunSuite with TableDrivenPropertyChecks {

  val rawChanges =
    """Alice would gain 54 happiness units by sitting next to Bob.
      |Alice would lose 79 happiness units by sitting next to Carol.
      |Alice would lose 2 happiness units by sitting next to David.
      |Bob would gain 83 happiness units by sitting next to Alice.
      |Bob would lose 7 happiness units by sitting next to Carol.
      |Bob would lose 63 happiness units by sitting next to David.
      |Carol would lose 62 happiness units by sitting next to Alice.
      |Carol would gain 60 happiness units by sitting next to Bob.
      |Carol would gain 55 happiness units by sitting next to David.
      |David would gain 46 happiness units by sitting next to Alice.
      |David would lose 7 happiness units by sitting next to Bob.
      |David would gain 41 happiness units by sitting next to Carol.
    """.stripMargin

  val changes = Seq(
    HappinessChange("Alice", "Bob",   54),
    HappinessChange("Alice", "Carol", -79),
    HappinessChange("Alice", "David", -2),
    HappinessChange("Bob",   "Alice", 83),
    HappinessChange("Bob",   "Carol", -7),
    HappinessChange("Bob",   "David", -63),
    HappinessChange("Carol", "Alice", -62),
    HappinessChange("Carol", "Bob",   60),
    HappinessChange("Carol", "David", 55),
    HappinessChange("David", "Alice", 46),
    HappinessChange("David", "Bob",   -7),
    HappinessChange("David", "Carol", 41))

  test("Day13 - Parse") {
    assert(rawChanges.split("\n").flatMap(parseHappinessChange).toList == changes)
  }

  test("Day13 - Part 1") {
    assert(part1(changes) == 330)
  }

  test("Day13 - Part 2") {
    assert(part2(changes) == 286)
  }
}
