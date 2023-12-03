package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.year2023.Day03Definitions._
import com.lmat.adventofcode.year2023.Day03._
import org.scalatest.funsuite.AnyFunSuite

class Day03Test extends AnyFunSuite {
  val rawMap =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin

  val schematic = parse(rawMap.split("\n").toList)
  val parts = List(
    Part(598, List(Coordinate(7, 9), Coordinate(6, 9), Coordinate(5, 9))),
    Part(664, List(Coordinate(3, 9), Coordinate(2, 9), Coordinate(1, 9))),
    Part(755, List(Coordinate(8, 7), Coordinate(7, 7), Coordinate(6, 7))),
    Part(592, List(Coordinate(4, 6), Coordinate(3, 6), Coordinate(2, 6))),
    Part(58, List(Coordinate(8, 5), Coordinate(7, 5))),
    Part(617, List(Coordinate(2, 4), Coordinate(1, 4), Coordinate(0, 4))),
    Part(633, List(Coordinate(8, 2), Coordinate(7, 2), Coordinate(6, 2))),
    Part(35, List(Coordinate(3, 2), Coordinate(2, 2))),
    Part(114, List(Coordinate(7, 0), Coordinate(6, 0), Coordinate(5, 0))),
    Part(467, List(Coordinate(2, 0), Coordinate(1, 0), Coordinate(0, 0)))
  )

  val gears = List(
    Gear(Coordinate(3, 1), List(35, 467)),
    Gear(Coordinate(3, 4), List(617)),
    Gear(Coordinate(5, 8), List(598, 755))
  )

  test("collectParts") {
    assert(collectParts(schematic) == parts)
  }

  test("collectGears") {
    assert(collectGears(schematic) == gears)
  }

  test("part1") {
    assert(part1(schematic) == 4361)
  }

  test("part2") {
    assert(part2(schematic) == 467835)
  }
}
