package com.lmat.adventofcode.year2021

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2021.Day04Definitions._
import com.lmat.adventofcode.year2021.Day04._
import com.lmat.util.Matrix

class Day04Test extends AnyFunSuite {

  val rawBingo =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7""".stripMargin

  val bingo =
    Bingo(
      5,
      List(
        Matrix(Vector(
          Vector(Cell(22, false), Cell(13, false), Cell(17, false), Cell(11, false), Cell(0,  false)),
          Vector(Cell(8,  false), Cell(2,  false), Cell(23, false), Cell(4,  false), Cell(24, false)),
          Vector(Cell(21, false), Cell(9,  false), Cell(14, false), Cell(16, false), Cell(7,  false)),
          Vector(Cell(6,  false), Cell(10, false), Cell(3,  false), Cell(18, false), Cell(5,  false)),
          Vector(Cell(1,  false), Cell(12, false), Cell(20, false), Cell(15, false), Cell(19, false)))),
        Matrix(Vector(
          Vector(Cell(3,  false), Cell(15, false), Cell(0,  false), Cell(2,  false), Cell(22, false)),
          Vector(Cell(9,  false), Cell(18, false), Cell(13, false), Cell(17, false), Cell(5,  false)),
          Vector(Cell(19, false), Cell(8,  false), Cell(7,  false), Cell(25, false), Cell(23, false)),
          Vector(Cell(20, false), Cell(11, false), Cell(10, false), Cell(24, false), Cell(4,  false)),
          Vector(Cell(14, false), Cell(21, false), Cell(16, false), Cell(12, false), Cell(6,  false)))),
        Matrix(Vector(
          Vector(Cell(14, false), Cell(21, false), Cell(17, false), Cell(24, false), Cell(4,  false)),
          Vector(Cell(10, false), Cell(16, false), Cell(15, false), Cell(9,  false), Cell(19, false)),
          Vector(Cell(18, false), Cell(8,  false), Cell(23, false), Cell(26, false), Cell(20, false)),
          Vector(Cell(22, false), Cell(11, false), Cell(13, false), Cell(6,  false), Cell(5,  false)),
          Vector(Cell(2,  false), Cell(0,  false), Cell(12, false), Cell(3,  false), Cell(7,  false))))
      ),
      List(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1),
      List(),
      List()
    )

  val last =
    Bingo(
      5,
      List(),
      List(6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1),
      List(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13),
      List(
        Matrix(Vector(
          Vector(Cell(14, true),  Cell(21, true),  Cell(17, true),  Cell(24, true),  Cell(4,  true)),
          Vector(Cell(10, false), Cell(16, false), Cell(15, false), Cell(9,  true),  Cell(19, false)),
          Vector(Cell(18, false), Cell(8,  false), Cell(23, true),  Cell(26, false), Cell(20, false)),
          Vector(Cell(22, false), Cell(11, true),  Cell(13, false), Cell(6,  false), Cell(5,  true)),
          Vector(Cell(2,  true),  Cell(0,  true),  Cell(12, false), Cell(3,  false), Cell(7,  true)))),
        Matrix(Vector(
          Vector(Cell(22, false), Cell(13, false), Cell(17, true),  Cell(11, true),  Cell(0,  true)),
          Vector(Cell(8,  false), Cell(2,  true),  Cell(23, true),  Cell(4,  true),  Cell(24, true)),
          Vector(Cell(21, true),  Cell(9,  true),  Cell(14, true),  Cell(16, true),  Cell(7,  true)),
          Vector(Cell(6,  false), Cell(10, true),  Cell(3,  false), Cell(18, false), Cell(5,  true)),
          Vector(Cell(1,  false), Cell(12, false), Cell(20, false), Cell(15, false), Cell(19, false)))),
        Matrix(Vector(
          Vector(Cell(3,  false), Cell(15, false), Cell(0,  true), Cell(2,  true),  Cell(22, false)),
          Vector(Cell(9,  true),  Cell(18, false), Cell(13, true), Cell(17, true),  Cell(5,  true)),
          Vector(Cell(19, false), Cell(8,  false), Cell(7,  true), Cell(25, false), Cell(23, true)),
          Vector(Cell(20, false), Cell(11, true),  Cell(10, true), Cell(24, true),  Cell(4,  true)),
          Vector(Cell(14, true),  Cell(21, true),  Cell(16, true), Cell(12, false), Cell(6,  false))))
      )
    )

  test("parseBingo") {
    assert(parseBingo(5)(rawBingo.split("\n").toList) == bingo)
  }

  test("last state") {
    assert(play(bingo).last == last)
  }

  test("part1") {
    assert(part1(bingo) == 4512)
  }

  test("part2") {
    assert(part2(bingo) == 1924)
  }
}
