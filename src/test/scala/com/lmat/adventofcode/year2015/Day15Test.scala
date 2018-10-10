package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day15._
import com.lmat.adventofcode.year2015.Day15Definitions.Ingredient
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day15Test extends FunSuite with TableDrivenPropertyChecks {

  val rawIngredients =
    """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
      |Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
    """.stripMargin

  val butterscotch = Ingredient("Butterscotch", -1, -2,  6,  3, 8)
  val cinnamon     = Ingredient("Cinnamon",      2,  3, -2, -1, 3)
  val ingredients  = List(butterscotch, cinnamon)

  test("Day15 - Parse") {
    assert(rawIngredients.split("\n").flatMap(parseIngredient).toSeq == ingredients)
  }

  val scores =
    Table(
      ("ingredients",                             "score"),
      (Map(butterscotch -> 100, cinnamon -> 0),   0),
      (Map(butterscotch -> 0,   cinnamon -> 100), 0),
      (Map(butterscotch -> 50,  cinnamon -> 50),  50000000),
      (Map(butterscotch -> 44,  cinnamon -> 56),  62842880),
      (Map(butterscotch -> 56,  cinnamon -> 44),  19681280)
    )

  test("Day15 - Scoring") {
    forAll(scores) { (ingredients, s) =>
      assert(score(ingredients) == s)
    }
  }

  test("Day15 - Part 1") {
    assert(part1(preProcess(ingredients)) == 62842880)
  }

  test("Day15 - Part 2") {
    assert(part2(preProcess(ingredients)) == 57600000)
  }
}
