package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.year2023.Day02Definitions._
import com.lmat.adventofcode.year2023.Day02._
import org.scalatest.funsuite.AnyFunSuite

class Day02Test extends AnyFunSuite {

  val rawGames =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin

  val games = List(
    Game(1, List(CubeSet(4, 0, 3), CubeSet(1, 2, 6), CubeSet(0, 2, 0))),
    Game(2, List(CubeSet(0, 2, 1), CubeSet(1, 3, 4), CubeSet(0, 1, 1))),
    Game(3, List(CubeSet(20, 8, 6), CubeSet(4, 13, 5), CubeSet(1, 5, 0))),
    Game(4, List(CubeSet(3, 1, 6), CubeSet(6, 3, 0), CubeSet(14, 3, 15))),
    Game(5, List(CubeSet(6, 3, 1), CubeSet(1, 2, 2)))
  )

  test("parse") {
    assert(rawGames.split("\n").flatMap(parseGame).toList == games)
  }

  test("part1") {
    assert(part1(games) == 8)
  }

  test("part2") {
    assert(part2(games) == 2286)
  }
}

