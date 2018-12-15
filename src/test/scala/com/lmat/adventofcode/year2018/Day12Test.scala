package com.lmat.adventofcode.year2018

import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2018.Day12Definitions._
import com.lmat.adventofcode.year2018.Day12._

class Day12Test extends FunSuite with TableDrivenPropertyChecks {

  val rawInput =
    """initial state: #..#.#..##......###...###
      |
      |...## => #
      |..#.. => #
      |.#... => #
      |.#.#. => #
      |.#.## => #
      |.##.. => #
      |.#### => #
      |#.#.# => #
      |#.### => #
      |##.#. => #
      |##.## => #
      |###.. => #
      |###.# => #
      |####. => #""".stripMargin

  val rawConfiguration = (
    Seq(true, false, false, true, false, true, false, false, true, true, false, false, false, false, false, false, true, true, true, false, false, false, true, true, true),
    Seq(
      Rule(Seq(false, false, false, true,  true),  true),
      Rule(Seq(false, false, true,  false, false), true),
      Rule(Seq(false, true,  false, false, false), true),
      Rule(Seq(false, true,  false, true,  false), true),
      Rule(Seq(false, true,  false, true,  true),  true),
      Rule(Seq(false, true,  true,  false, false), true),
      Rule(Seq(false, true,  true,  true,  true),  true),
      Rule(Seq(true,  false, true,  false, true),  true),
      Rule(Seq(true,  false, true,  true,  true),  true),
      Rule(Seq(true,  true,  false, true,  false), true),
      Rule(Seq(true,  true,  false, true,  true),  true),
      Rule(Seq(true,  true,  true,  false, false), true),
      Rule(Seq(true,  true,  true,  false, true),  true),
      Rule(Seq(true,  true,  true,  true,  false), true)))

  test("Day12 - Parse") {
    assert(parseInput(rawInput.split("\n")) == rawConfiguration)
  }

  val configuration = (
    Set(0, 3, 5, 8, 9, 16, 17, 18, 22, 23, 24),
    Set(
      Seq(true,  true,  true,  false, true),
      Seq(false, true,  false, true,  true),
      Seq(true,  false, true,  false, true),
      Seq(true,  true,  false, true,  false),
      Seq(false, true,  true,  true,  true),
      Seq(false, true,  false, true,  false),
      Seq(true,  false, true,  true,  true),
      Seq(true,  true,  true,  true,  false),
      Seq(false, true,  true,  false, false),
      Seq(true,  true,  false, true,  true),
      Seq(true,  true,  true,  false, false),
      Seq(false, false, true,  false, false),
      Seq(false, false, false, true,  true),
      Seq(false, true,  false, false, false)))

  test("Day12 - PreProcess") {
    assert(preProcess(rawConfiguration) == configuration)
  }

  test("Day12 - Next") {
    assert(next(configuration._2)(configuration._1) == Set(0, 4, 9, 15, 18, 21, 24))
    assert(next(configuration._2)(Set(0, 4, 9, 15, 18, 21, 24)) == Set(0, 1, 4, 5, 9, 10, 15, 18, 21, 24, 25))
    assert(next(configuration._2)(Set(0, 1, 4, 5, 9, 10, 15, 18, 21, 24, 25)) == Set(-1, 1, 5, 8, 10, 15, 18, 21, 25))
  }

  test("Day12 - Simulate") {
    assert(simulate(configuration._2, 20)(configuration._1) == Set(-2, 3, 4, 9, 10, 11, 12, 13, 17, 18, 19, 20, 21, 22, 23, 28, 30, 33, 34))
  }

  test("Day12 - Part 1") {
    assert(part1(configuration) == 325)
  }
}
