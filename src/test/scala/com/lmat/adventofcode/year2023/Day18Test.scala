package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day18Definitions._
import com.lmat.adventofcode.year2023.Day18._
class Day18Test extends AnyFunSuite {
  val raw =
    """R 6 (#70c710)
      |D 5 (#0dc571)
      |L 2 (#5713f0)
      |D 2 (#d2c081)
      |R 2 (#59c680)
      |D 2 (#411b91)
      |L 5 (#8ceee2)
      |U 2 (#caa173)
      |L 1 (#1b58a2)
      |U 2 (#caa171)
      |R 2 (#7807d2)
      |U 3 (#a77fa3)
      |L 2 (#015232)
      |U 2 (#7a21e3)""".stripMargin

  val digPlan = List(
    Plan(East, 6),
    Plan(South, 5),
    Plan(West, 2),
    Plan(South, 2),
    Plan(East, 2),
    Plan(South, 2),
    Plan(West, 5),
    Plan(North, 2),
    Plan(West, 1),
    Plan(North, 2),
    Plan(East, 2),
    Plan(North, 3),
    Plan(West, 2),
    Plan(North, 2)
  )

  val digPlan2 = List(
    Plan(East, 461937),
    Plan(South, 56407),
    Plan(East, 356671),
    Plan(South, 863240),
    Plan(East, 367720),
    Plan(South, 266681),
    Plan(West, 577262),
    Plan(North, 829975),
    Plan(West, 112010),
    Plan(South, 829975),
    Plan(West, 491645),
    Plan(North, 686074),
    Plan(West, 5411),
    Plan(North, 500254)
  )

  test("parse"){
    assert(raw.split("\n").flatMap(parsePlan).toList == digPlan)
    assert(raw.split("\n").flatMap(parsePlan2).toList == digPlan2)
  }

  test("part1") {
    assert(part1(digPlan) == 62)
  }

  test("part2") {
    assert(part2(digPlan2) == 952408144115L)
  }
}
