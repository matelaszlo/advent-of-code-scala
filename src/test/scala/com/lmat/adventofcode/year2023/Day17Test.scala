package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day17._
import com.lmat.adventofcode.year2023.Day17Definitions._

class Day17Test extends AnyFunSuite {

  val raw =
    """2413432311323
      |3215453535623
      |3255245654254
      |3446585845452
      |4546657867536
      |1438598798454
      |4457876987766
      |3637877979653
      |4654967986887
      |4564679986453
      |1224686865563
      |2546548887735
      |4322674655533""".stripMargin

  val raw2 =
    """111111111111
      |999999999991
      |999999999991
      |999999999991
      |999999999991""".stripMargin

  val cityMap = parseCityMap(raw.split("\n").toList)
  val cityMap2 = parseCityMap(raw2.split("\n").toList)

  test("parse") {
    assert(print(cityMap) == raw)
    assert(print(cityMap2) == raw2)
  }

  test("part1") {
    assert(part1(cityMap) == 102)
  }

  test("part2") {
    assert(part2(cityMap) == 94)
    assert(part2(cityMap2) == 71)
  }
}
