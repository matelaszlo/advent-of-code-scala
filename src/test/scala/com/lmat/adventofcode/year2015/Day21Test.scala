package com.lmat.adventofcode.year2015

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2015.Day21Definitions._
import com.lmat.adventofcode.year2015.Day21._


class Day21Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val games =
    Table(
      ("player",            "boss",              "damage", "canWin"),
      (Character(8, 5, 5),  Character(12, 7, 2), 3,        true),
      (Character(8, 5, 4),  Character(12, 7, 2), 3,        false),
      (Character(8, 4, 5),  Character(12, 7, 2), 2,        false),
      (Character(12, 7, 2), Character(8, 5, 5),  2,        true), // Player wins here as well because of starters advantage
      (Character(12, 6, 2), Character(8, 5, 5),  1,        false),
      (Character(12, 7, 1), Character(8, 5, 5),  2,        false),
      (Character(12, 5, 2), Character(8, 5, 5),  1,        false),
      (Character(12, 4, 2), Character(8, 5, 5),  1,        false),
    )

  test("Day21 - Damage") {
    forAll(games) { (player, boss, damage, _) =>
      assert(calculateDamage(player, boss) == damage)
    }
  }

  test("Day21 - Can Win") {
    forAll(games) { (player, boss, _, win) =>
      assert(canWin(player, boss) == win)
    }
  }
}
