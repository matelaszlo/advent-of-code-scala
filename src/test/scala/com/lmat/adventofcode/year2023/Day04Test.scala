package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day04Definitions._
import com.lmat.adventofcode.year2023.Day04._

class Day04Test extends AnyFunSuite {

  val rawGames =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
      |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
      |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
      |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
      |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
      |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin

  val games = List(
    Game(1, List(41, 48, 83, 86, 17), List(83, 86, 6, 31, 17, 9, 48, 53)),
    Game(2, List(13, 32, 20, 16, 61), List(61, 30, 68, 82, 17, 32, 24, 19)),
    Game(3, List(1, 21, 53, 59, 44), List(69, 82, 63, 72, 16, 21, 14, 1)),
    Game(4, List(41, 92, 73, 84, 69), List(59, 84, 76, 51, 58, 5, 54, 83)),
    Game(5, List(87, 83, 26, 28, 32), List(88, 30, 70, 12, 93, 22, 82, 36)),
    Game(6, List(31, 18, 13, 56, 72), List(74, 77, 10, 23, 35, 67, 36, 11))
  )

  test("parse") {
    assert(rawGames.split("\n").flatMap(parseGame).toList == games)
  }

  test("part1") {
    assert(part1(games) == 13)
  }

  test("simulate") {
    assert(simulate(start(games)).finished == Map(1 -> 1, 6 -> 1, 2 -> 2, 3 -> 4, 4 -> 8, 5 -> 14))
  }

  test("part2") {
    assert(part2(games) == 30)
  }
}
