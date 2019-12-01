package com.lmat.adventofcode.year2018

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2018.Day13Definitions._
import com.lmat.adventofcode.year2018.Day13._

class Day13Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawInput: String =
    """/->-\
      ||   |  /----\
      || /-+--+-\  |
      || | |  | v  |
      |\-+-/  \-+--/
      |  \------/   """.stripMargin

  val tracks: Tracks = Map(
    (0, 0) -> CurveRight, (1, 0) -> Straight, (2, 0) -> Straight, (3, 0) -> Straight, (4, 0) -> CurveLeft,
    (0, 1) -> Straight, (1, 1) -> Empty, (2, 1) -> Empty, (3, 1) -> Empty, (4, 1) -> Straight, (5, 1) -> Empty, (6, 1) -> Empty, (7, 1) -> CurveRight, (8, 1) -> Straight, (9, 1) -> Straight, (10, 1) -> Straight, (11, 1) -> Straight, (12, 1) -> CurveLeft,
    (0, 2) -> Straight, (1, 2) -> Empty, (2, 2) -> CurveRight, (3, 2) -> Straight, (4, 2) -> Crossing, (5, 2) -> Straight, (6, 2) -> Straight, (7, 2) -> Crossing, (8, 2) -> Straight, (9, 2) -> CurveLeft, (10, 2) -> Empty, (11, 2) -> Empty, (12, 2) -> Straight,
    (0, 3) -> Straight, (1, 3) -> Empty, (2, 3) -> Straight, (3, 3) -> Empty, (4, 3) -> Straight, (5, 3) -> Empty, (6, 3) -> Empty, (7, 3) -> Straight, (8, 3) -> Empty, (9, 3) -> Straight, (10, 3) -> Empty, (11, 3) -> Empty, (12, 3) -> Straight,
    (0, 4) -> CurveLeft, (1, 4) -> Straight, (2, 4) -> Crossing, (3, 4) -> Straight, (4, 4) -> CurveRight, (5, 4) -> Empty, (6, 4) -> Empty, (7, 4) -> CurveLeft, (8, 4) -> Straight, (9, 4) -> Crossing, (10, 4) -> Straight, (11, 4) -> Straight, (12, 4) -> CurveRight,
    (0, 5) -> Empty, (1, 5) -> Empty, (2, 5) -> CurveLeft, (3, 5) -> Straight, (4, 5) -> Straight, (5, 5) -> Straight, (6, 5) -> Straight, (7, 5) -> Straight, (8, 5) -> Straight, (9, 5) -> CurveRight, (10, 5) -> Empty, (11, 5) -> Empty, (12, 5) -> Empty
  )

  val carts: Seq[Cart] = Seq(
    Cart((2, 0), Right, TurnLeft),
    Cart((9, 3), Down, TurnLeft))

  test("Day13 - Parse") {
    val (parsedTracks, parsedCarts) = parseInput(rawInput.split("\n"))
    assert(parsedTracks == tracks)
    assert(parsedCarts == carts)
  }

  val rawInput2: String =
    """/>-<\
      ||   |
      || /<+-\
      || | | v
      |\>+</ |
      |  |   ^
      |  \<->/""".stripMargin

  test("Day13 - Part 1") {
    assert(part1((tracks, carts)) == "7,3")
  }

  test("Day13 - Part 2") {
    val (parsedTracks, parsedCarts) = parseInput(rawInput2.split("\n"))
    assert(part2((parsedTracks, parsedCarts)) == "6,4")
  }
}
