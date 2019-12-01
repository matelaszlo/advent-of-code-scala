package com.lmat.adventofcode.year2018

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2018.Day24Definitions._
import com.lmat.adventofcode.year2018.Day24._

class Day24Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawGroupConfiguration =
    """Immune System:
      |17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
      |989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3
      |
      |Infection:
      |801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
      |4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4""".stripMargin

  val groupConfiguration = Map(
    ("immune", 0) -> Group(17,   5390, 4507, "fire",        2, Seq("radiation", "bludgeoning"), Seq()),
    ("immune", 1) -> Group(989,  1274, 25,   "slashing",    3, Seq("bludgeoning", "slashing"),  Seq("fire")),
    ("infect", 0) -> Group(801,  4706, 116,  "bludgeoning", 1, Seq("radiation"),                Seq()),
    ("infect", 1) -> Group(4485, 2961, 12,   "slashing",    4, Seq("fire", "cold"),             Seq("radiation"))
  )

  test("Day24 - Parse") {
    assert(parseGroupConfiguration(rawGroupConfiguration.split("\n").toIndexedSeq) == groupConfiguration)
  }

  test("Day24 - Part 1") {
    assert(part1(groupConfiguration) == 5216)
  }

  test("Day24 - Part 2") {
    assert(part2(groupConfiguration) == 51)
  }
}
