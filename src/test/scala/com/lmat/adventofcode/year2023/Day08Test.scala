package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day08Definitions._
import com.lmat.adventofcode.year2023.Day08._

class Day08Test extends AnyFunSuite {
  val raw =
    """RL
      |
      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  val raw2 =
    """LLR
      |
      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  val raw3 =
    """LR
      |
      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)""".stripMargin

  val parsed =
    Network(
      List(Right, Left),
      Map(
        "AAA" -> Map(Left -> "BBB", Right -> "CCC"),
        "BBB" -> Map(Left -> "DDD", Right -> "EEE"),
        "CCC" -> Map(Left -> "ZZZ", Right -> "GGG"),
        "DDD" -> Map(Left -> "DDD", Right -> "DDD"),
        "EEE" -> Map(Left -> "EEE", Right -> "EEE"),
        "GGG" -> Map(Left -> "GGG", Right -> "GGG"),
        "ZZZ" -> Map(Left -> "ZZZ", Right -> "ZZZ"),
      ))

  val parsed2 = Network(
    List(Left, Left, Right),
    Map(
      "AAA" -> Map(Left -> "BBB", Right -> "BBB"),
      "BBB" -> Map(Left -> "AAA", Right -> "ZZZ"),
      "ZZZ" -> Map(Left -> "ZZZ", Right -> "ZZZ")
    ))

  val parsed3 = Network(
    List(Left, Right),
    Map(
      "11A" -> Map(Left -> "11B", Right -> "XXX"),
      "11B" -> Map(Left -> "XXX", Right -> "11Z"),
      "11Z" -> Map(Left -> "11B", Right -> "XXX"),
      "22A" -> Map(Left -> "22B", Right -> "XXX"),
      "22B" -> Map(Left -> "22C", Right -> "22C"),
      "22C" -> Map(Left -> "22Z", Right -> "22Z"),
      "22Z" -> Map(Left -> "22B", Right -> "22B"),
      "XXX" -> Map(Left -> "XXX", Right -> "XXX"),
    ))

  test("parse") {
    assert(parseNetwork(raw.split("\n").toList) == parsed)
    assert(parseNetwork(raw2.split("\n").toList) == parsed2)
    assert(parseNetwork(raw3.split("\n").toList) == parsed3)
  }

  test("part1") {
    assert(part1(parsed) == 2)
    assert(part1(parsed2) == 6)
  }

  test("part2") {
    assert(part2(parsed3) == 6)
  }
}