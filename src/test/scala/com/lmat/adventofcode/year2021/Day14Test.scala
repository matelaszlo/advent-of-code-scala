package com.lmat.adventofcode.year2021

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2021.Day14._

class Day14Test extends AnyFunSuite {
  val input =
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C""".stripMargin

  val template = "NNCB"
  val rules = Map("BB" -> "N", "HH" -> "N", "HC" -> "B", "BN" -> "B", "BC" -> "B", "CN" -> "C", "BH" -> "H", "NN" -> "C", "CC" -> "N", "CH" -> "B", "HB" -> "C", "CB" -> "H", "HN" -> "C", "NH" -> "C", "NC" -> "B", "NB" -> "B")

  test("parseInstructions") {
    assert(parseInstructions(input.split("\n")) == (template, rules))
  }

  test("polymerization") {
    assert(polymerization(rules)(template).take(5).toList == List("NNCB", "NCNBCHB", "NBCCNBBBCBHCB", "NBBBCNCCNBBNBNBBCHBHHBCHB", "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"))
  }

  test("polymerization2") {
    assert(polymerization2(rules)(template).take(3).toList == List(
      Map("NN" -> 1, "NC" -> 1, "CB" -> 1),
      Map("NC" -> 1, "CN" -> 1, "NB" -> 1, "BC" -> 1, "CH" -> 1, "HB" -> 1),
      Map("NB" -> 2, "BC" -> 2, "CC" -> 1, "CN" -> 1, "BB" -> 2, "CB" -> 2, "BH" -> 1, "HC" -> 1)
    ))
  }

  test("leastAndMostCommon") {
    assert(leastAndMostCommon(polymerization(rules)(template).drop(10).head) == (('H', 161), ('B', 1749)))
  }

  test("leastAndMostCommon2") {
    assert(leastAndMostCommon2("NN")(polymerization2(rules)(template).drop(10).head) == (('H', 161), ('B', 1749)))
    assert(leastAndMostCommon2("NN")(polymerization2(rules)(template).drop(40).head) == (('H', BigInt("3849876073")), ('B', BigInt("2192039569602"))))
  }

  test("part1") {
    assert(part1((template, rules)) == 1588)
  }

  test("part2") {
    assert(part2((template, rules)) == BigInt(2188189693529L))
  }
}
