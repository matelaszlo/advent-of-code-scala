package com.lmat.adventofcode.year2021

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2021.Day13._
import com.lmat.adventofcode.year2021.Day13Definitions._

class Day13Test extends AnyFunSuite {
  val rawInstructions =
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5""".stripMargin

  val dots = Set(Dot(0, 3), Dot(6, 12), Dot(6, 10), Dot(8, 10), Dot(10, 12), Dot(9, 10), Dot(9, 0), Dot(10, 4), Dot(3, 4), Dot(1, 10), Dot(8, 4), Dot(4, 11), Dot(4, 1), Dot(3, 0), Dot(0, 13), Dot(6, 0), Dot(2, 14), Dot(0, 14))
  val folds = List(Fold("y", 7), Fold("x", 5))

  test("parseInstructions") {
    assert(parseInstructions(rawInstructions.split("\n")) == (dots, folds))
  }

  test("foldPaper") {
    println(draw(dots))
    val foldUp = foldPaper(dots, folds.head)
    println(draw(foldUp))
    val foldLeft = foldPaper(foldUp, folds(1))
    println(draw(foldLeft))
  }

  test("part1") {
    assert(part1(dots, folds) == 17)
  }
}
