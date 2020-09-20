package com.lmat.adventofcode.year2018

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2018.Day17._
import com.lmat.adventofcode.year2018.Day17Definitions._

class Day17Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawClayCoordinates =
    """x=495, y=2..7
      |y=7, x=495..501
      |x=501, y=3..7
      |x=498, y=2..4
      |x=506, y=1..2
      |x=498, y=10..13
      |x=504, y=10..13
      |y=13, x=498..504""".stripMargin

  val rawProblematic1 =
    """y=11, x=494..508
      |x=508, y=0..11
      |x=494, y=0..11
      |y=8, x=499..502
      |x=502, y=5..8
      |x=499, y=5..8
      |y=5, x=499..502""".stripMargin

  val rawProblematic2 =
    """y=4, x=498..502
      |y=10, x=494..500
      |y=20, x=490..510
      |x=490, y=8..20
      |x=510, y=8..20
      |x=494, y=6..10
      |x=500, y=6..10""".stripMargin

  val rawProblematic3 =
    """y=4, x=491..508
      |y=8, x=494..500
      |y=10, x=490..510
      |x=490, y=8..10
      |x=510, y=8..10
      |x=494, y=6..8
      |x=500, y=6..8""".stripMargin

  test("Day17 - Parse Coordinates") {
    val expectedCoordinates = Set(
      ClayCoordinatesHorizontal(495, 2, 7),
      ClayCoordinatesVertical(7, 495, 501),
      ClayCoordinatesHorizontal(501, 3, 7),
      ClayCoordinatesHorizontal(498, 2, 4),
      ClayCoordinatesHorizontal(506, 1, 2),
      ClayCoordinatesHorizontal(498, 10, 13),
      ClayCoordinatesHorizontal(504, 10, 13),
      ClayCoordinatesVertical(13, 498, 504))
    assert(rawClayCoordinates.split("\n").flatMap(parseRow).toSet == expectedCoordinates)
  }

  test("Day17 - Coordinates to State") {
    val expectedSate = ReservoirState(Map(
      (Coordinate(495, 2), Clay), (Coordinate(495, 3), Clay), (Coordinate(495, 4), Clay), (Coordinate(495, 5), Clay), (Coordinate(495, 6), Clay), (Coordinate(495, 7), Clay),
      (Coordinate(496, 7), Clay),
      (Coordinate(497, 7), Clay),
      (Coordinate(498, 2), Clay), (Coordinate(498, 3), Clay), (Coordinate(498, 4), Clay), (Coordinate(498, 7), Clay), (Coordinate(498, 10), Clay), (Coordinate(498, 11), Clay), (Coordinate(498, 12), Clay), (Coordinate(498, 13), Clay),
      (Coordinate(499, 7), Clay), (Coordinate(499, 13), Clay),
      (Coordinate(500, 7), Clay), (Coordinate(500, 13), Clay),
      (Coordinate(501, 3), Clay), (Coordinate(501, 4), Clay), (Coordinate(501, 5), Clay), (Coordinate(501, 6), Clay), (Coordinate(501, 7), Clay), (Coordinate(501, 13), Clay),
      (Coordinate(502, 13), Clay),
      (Coordinate(503, 13), Clay),
      (Coordinate(504, 10), Clay), (Coordinate(504, 11), Clay), (Coordinate(504, 12), Clay), (Coordinate(504, 13), Clay),
      (Coordinate(506, 1), Clay), (Coordinate(506, 2), Clay)))
    val clayCoordinates = rawClayCoordinates.split("\n").flatMap(parseRow).toSet
    assert(build(clayCoordinates) == expectedSate)
  }

  val simulations = Table(
    ("coordinates", "part1", "part2"),
    (rawClayCoordinates, 57, 29),
    (rawProblematic1, 151, 127),
    (rawProblematic2, 284, 227),
    (rawProblematic3, 59, 31),
  )

  test("Day17 - Parts") {
    forAll(simulations) { (coordinates, part1Result, part2Result) =>
      val simulated = preProcess(coordinates.split("\n").flatMap(parseRow).toSet)
      assert(part1(simulated) == part1Result)
      assert(part2(simulated) == part2Result)
    }
  }
}
