package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day14._
import com.lmat.adventofcode.year2023.Day14Definitions._

class Day14Test extends AnyFunSuite {
  val raw =
    """O....#....
      |O.OO#....#
      |.....##...
      |OO.#O....O
      |.O.....O#.
      |O.#..O.#.#
      |..O..#O..O
      |.......O..
      |#....###..
      |#OO..#....""".stripMargin

  val rawTilted =
    """OOOO.#.O..
      |OO..#....#
      |OO..O##..O
      |O..#.OO...
      |........#.
      |..#....#.#
      |..O..#.O.O
      |..O.......
      |#....###..
      |#....#....""".stripMargin

  val rawCycle1 =
    """.....#....
      |....#...O#
      |...OO##...
      |.OO#......
      |.....OOO#.
      |.O#...O#.#
      |....O#....
      |......OOOO
      |#...O###..
      |#..OO#....""".stripMargin

  val rawCycle2 =
    """.....#....
      |....#...O#
      |.....##...
      |..O#......
      |.....OOO#.
      |.O#...O#.#
      |....O#...O
      |.......OOO
      |#..OO###..
      |#.OOO#...O""".stripMargin

  val rawCycle3 =
    """.....#....
      |....#...O#
      |.....##...
      |..O#......
      |.....OOO#.
      |.O#...O#.#
      |....O#...O
      |.......OOO
      |#...O###.O
      |#.OOO#...O""".stripMargin

  val dish = parseDish(raw.split("\n").toList)
  val dishNorth = parseDish(rawTilted.split("\n").toList)
  val cycle1 = parseDish(rawCycle1.split("\n").toList)
  val cycle2 = parseDish(rawCycle2.split("\n").toList)
  val cycle3 = parseDish(rawCycle3.split("\n").toList)

  test("parse") {
    assert(print(parseDish(raw.split("\n").toList)) == raw)
  }

  test("tilt") {
    assert(tilt(dish, North) == dishNorth)
  }

  test("cycle") {
    assert(cycle(dish) == cycle1)
    assert(cycle(cycle1) == cycle2)
    assert(cycle(cycle2) == cycle3)
  }

  test("part1") {
    assert(part1(dish) == 136)
  }

  test("part2") {
    assert(part2(dish) == 64)
  }
}
