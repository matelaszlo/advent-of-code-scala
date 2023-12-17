package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day16._
import com.lmat.adventofcode.year2023.Day16Definitions._

class Day16Test extends AnyFunSuite {

  val raw =
    """.|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....""".stripMargin

  val contraption = parseContraption(raw.split("\n").toList)

  test("parse") {
    assert(print(contraption) == raw)
  }

  test("energize") {
    assert(energize(contraption, Coordinate(0,0), East).toList.sorted == List(
      Coordinate(0, 0), Coordinate(1, 0), Coordinate(2, 0), Coordinate(3, 0), Coordinate(4, 0), Coordinate(5, 0),
      Coordinate(1, 1), Coordinate(5, 1),
      Coordinate(1, 2), Coordinate(5, 2), Coordinate(6, 2), Coordinate(7, 2), Coordinate(8, 2), Coordinate(9, 2),
      Coordinate(1, 3), Coordinate(5, 3), Coordinate(6, 3),
      Coordinate(1, 4), Coordinate(5, 4), Coordinate(6, 4),
      Coordinate(1, 5), Coordinate(5, 5), Coordinate(6, 5),
      Coordinate(1, 6), Coordinate(4, 6), Coordinate(5, 6), Coordinate(6, 6), Coordinate(7, 6),
      Coordinate(0, 7), Coordinate(1, 7), Coordinate(2, 7), Coordinate(3, 7), Coordinate(4, 7), Coordinate(5, 7), Coordinate(6, 7), Coordinate(7, 7),
      Coordinate(1, 8), Coordinate(2, 8), Coordinate(3, 8), Coordinate(4, 8), Coordinate(5, 8), Coordinate(6, 8), Coordinate(7, 8),
      Coordinate(1, 9), Coordinate(5, 9), Coordinate(7, 9)))
  }

  test("part1") {
    assert(part1(contraption) == 46)
  }

  test("part2") {
    assert(part2(contraption) == 51)
  }
}
