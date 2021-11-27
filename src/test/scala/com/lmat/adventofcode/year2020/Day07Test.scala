package com.lmat.adventofcode.year2020

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2020.Day07._
import com.lmat.adventofcode.year2020.Day07Definitions._

class Day07Test extends AnyFunSuite {

  // Example 1
  val rawBagRules =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.""".stripMargin

  val bagRules: BagRules =
    Map(
      "light red"    -> Map("bright white" -> 1, "muted yellow" -> 2),
      "dark orange"  -> Map("bright white" -> 3, "muted yellow" -> 4),
      "bright white" -> Map("shiny gold"   -> 1),
      "muted yellow" -> Map("shiny gold"   -> 2, "faded blue"   -> 9),
      "shiny gold"   -> Map("dark olive"   -> 1, "vibrant plum" -> 2),
      "dark olive"   -> Map("faded blue"   -> 3, "dotted black" -> 4),
      "vibrant plum" -> Map("faded blue"   -> 5, "dotted black" -> 6),
      "faded blue"   -> Map(),
      "dotted black" -> Map()
    )

  val bagCountMap: Map[Bag, Int] =
    Map(
      "faded blue"   -> 0,
      "dotted black" -> 0,
      "dark olive"   -> 7,
      "vibrant plum" -> 11,
      "shiny gold"   -> 32,
      "bright white" -> 33,
      "muted yellow" -> 75,
      "light red"    -> 186,
      "dark orange"  -> 406
    )

  // Example 2
  val rawBagRules2 =
    """shiny gold bags contain 2 dark red bags.
      |dark red bags contain 2 dark orange bags.
      |dark orange bags contain 2 dark yellow bags.
      |dark yellow bags contain 2 dark green bags.
      |dark green bags contain 2 dark blue bags.
      |dark blue bags contain 2 dark violet bags.
      |dark violet bags contain no other bags.""".stripMargin

  val bagRules2: BagRules =
    Map(
      "shiny gold"  -> Map("dark red"    -> 2),
      "dark red"    -> Map("dark orange" -> 2),
      "dark orange" -> Map("dark yellow" -> 2),
      "dark yellow" -> Map("dark green"  -> 2),
      "dark green"  -> Map("dark blue"   -> 2),
      "dark blue"   -> Map("dark violet" -> 2),
      "dark violet" -> Map()
    )

  val bagCountMap2: Map[Bag, Int] =
    Map(
      "dark violet" -> 0,
      "dark blue"   -> 2,
      "dark green"  -> 6,
      "dark yellow" -> 14,
      "dark orange" -> 30,
      "dark red"    -> 62,
      "shiny gold"  -> 126,
    )

  test("parse"){
    assert(parseBagRules(rawBagRules) == bagRules)
    assert(parseBagRules(rawBagRules2) == bagRules2)
  }

  test("findPossibleContainers"){
    assert(findPossibleContainers(bagRules)("shiny gold") == Set("light red", "bright white", "muted yellow", "dark orange"))
  }

  test("part1"){
    assert(part1(bagRules) == 4)
  }

  test("buildBagCountMap"){
    assert(buildBagCountMap(bagRules) == bagCountMap)
    assert(buildBagCountMap(bagRules2) == bagCountMap2)
  }

  test("part2"){
    assert(part2(bagRules) == 32)
    assert(part2(bagRules2) == 126)
  }
}
