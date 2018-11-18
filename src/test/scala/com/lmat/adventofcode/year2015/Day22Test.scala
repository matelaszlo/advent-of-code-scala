package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day22._
import com.lmat.adventofcode.year2015.Day22Definitions._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day22Test extends FunSuite with TableDrivenPropertyChecks {
  val effectTests =
    Table(
      ("player",          "boss",            "effects",                                "playerAfter",      "bossAfter",       "effectsAfter"),
      (Wizard(10, 21, 0), Warrior(14, 8, 0), Set[Effect](Mana(101, 5)),                Wizard(10, 122, 0), Warrior(14, 8, 0), Set[Effect](Mana(101, 4))),
      (Wizard(2, 110, 7), Warrior(14, 8, 0), Set[Effect](Mana(101, 3), ArmorUp(7, 5)), Wizard(2, 211, 7),  Warrior(14, 8, 0), Set[Effect](Mana(101, 2), ArmorUp(7, 4))),
      (Wizard(2, 110, 7), Warrior(14, 8, 0), Set[Effect](Mana(101, 1), ArmorUp(7, 1)), Wizard(2, 211, 0),  Warrior(14, 8, 0), Set[Effect]()),
    )

  test("Day22 - Damage") {
    forAll(effectTests) { (player, boss, effects, playerAfter, bossAfter, effectsAfter) =>
      assert(applyEffects(GameState(player, boss, effects)) == GameState(playerAfter, bossAfter, effectsAfter))
    }
  }

  val spellTest =
    Table(
      ("player",           "boss",            "spell",               "playerAfter",       "bossAfter",       "effectsAfter"),
      (Wizard(10, 250, 0), Warrior(14, 8, 0), MagicMissile(53, 4),   Wizard(10, 197, 0),  Warrior(10, 8, 0), Set[Effect]()),
      (Wizard(10, 250, 0), Warrior(14, 8, 0), Drain(73, 2),          Wizard(12, 177, 0),  Warrior(12, 8, 0), Set[Effect]()),
      (Wizard(10, 250, 0), Warrior(14, 8, 0), Shield(113, 7 ,6),     Wizard(10, 137, 7),  Warrior(14, 8, 0), Set[Effect](ArmorUp(7,6))),
      (Wizard(10, 250, 0), Warrior(14, 8, 0), Poison(173, 3, 6),     Wizard(10, 77, 0),   Warrior(14, 8, 0), Set[Effect](Damage(3,6))),
      (Wizard(10, 250, 0), Warrior(14, 8, 0), Recharge(229, 101, 5), Wizard(10, 21, 0),   Warrior(14, 8, 0), Set[Effect](Mana(101,5))),
    )

  test("Day22 - Spells") {
    forAll(spellTest) { (player, boss, spell, playerAfter, bossAfter, effectsAfter) =>
      assert(useSpell(player, boss, spell) == GameState(playerAfter, bossAfter, effectsAfter))
    }
  }
}
