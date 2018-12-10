package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day10Definitions._
import com.lmat.adventofcode.year2018.Day10._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day10Test extends FunSuite with TableDrivenPropertyChecks {

  val rawPoints =
    """position=< 9,  1> velocity=< 0,  2>
      |position=< 7,  0> velocity=<-1,  0>
      |position=< 3, -2> velocity=<-1,  1>
      |position=< 6, 10> velocity=<-2, -1>
      |position=< 2, -4> velocity=< 2,  2>
      |position=<-6, 10> velocity=< 2, -2>
      |position=< 1,  8> velocity=< 1, -1>
      |position=< 1,  7> velocity=< 1,  0>
      |position=<-3, 11> velocity=< 1, -2>
      |position=< 7,  6> velocity=<-1, -1>
      |position=<-2,  3> velocity=< 1,  0>
      |position=<-4,  3> velocity=< 2,  0>
      |position=<10, -3> velocity=<-1,  1>
      |position=< 5, 11> velocity=< 1, -2>
      |position=< 4,  7> velocity=< 0, -1>
      |position=< 8, -2> velocity=< 0,  1>
      |position=<15,  0> velocity=<-2,  0>
      |position=< 1,  6> velocity=< 1,  0>
      |position=< 8,  9> velocity=< 0, -1>
      |position=< 3,  3> velocity=<-1,  1>
      |position=< 0,  5> velocity=< 0, -1>
      |position=<-2,  2> velocity=< 2,  0>
      |position=< 5, -2> velocity=< 1,  2>
      |position=< 1,  4> velocity=< 2,  1>
      |position=<-2,  7> velocity=< 2, -2>
      |position=< 3,  6> velocity=<-1, -1>
      |position=< 5,  0> velocity=< 1,  0>
      |position=<-6,  0> velocity=< 2,  0>
      |position=< 5,  9> velocity=< 1, -2>
      |position=<14,  7> velocity=<-2,  0>
      |position=<-3,  6> velocity=< 2, -1>""".stripMargin

  val points = Seq(
    Point(Coordinate(9, 1), Coordinate(0, 2)),
    Point(Coordinate(7, 0), Coordinate(-1, 0)),
    Point(Coordinate(3, -2), Coordinate(-1, 1)),
    Point(Coordinate(6, 10), Coordinate(-2, -1)),
    Point(Coordinate(2, -4), Coordinate(2, 2)),
    Point(Coordinate(-6, 10), Coordinate(2, -2)),
    Point(Coordinate(1, 8), Coordinate(1, -1)),
    Point(Coordinate(1, 7), Coordinate(1, 0)),
    Point(Coordinate(-3, 11), Coordinate(1, -2)),
    Point(Coordinate(7, 6), Coordinate(-1, -1)),
    Point(Coordinate(-2, 3), Coordinate(1, 0)),
    Point(Coordinate(-4, 3), Coordinate(2, 0)),
    Point(Coordinate(10, -3), Coordinate(-1, 1)),
    Point(Coordinate(5, 11), Coordinate(1, -2)),
    Point(Coordinate(4, 7), Coordinate(0, -1)),
    Point(Coordinate(8, -2), Coordinate(0, 1)),
    Point(Coordinate(15, 0), Coordinate(-2, 0)),
    Point(Coordinate(1, 6), Coordinate(1, 0)),
    Point(Coordinate(8, 9), Coordinate(0, -1)),
    Point(Coordinate(3, 3), Coordinate(-1, 1)),
    Point(Coordinate(0, 5), Coordinate(0, -1)),
    Point(Coordinate(-2, 2), Coordinate(2, 0)),
    Point(Coordinate(5, -2), Coordinate(1, 2)),
    Point(Coordinate(1, 4), Coordinate(2, 1)),
    Point(Coordinate(-2, 7), Coordinate(2, -2)),
    Point(Coordinate(3, 6), Coordinate(-1, -1)),
    Point(Coordinate(5, 0), Coordinate(1, 0)),
    Point(Coordinate(-6, 0), Coordinate(2, 0)),
    Point(Coordinate(5, 9), Coordinate(1, -2)),
    Point(Coordinate(14, 7), Coordinate(-2, 0)),
    Point(Coordinate(-3, 6), Coordinate(2, -1)))

  test("Day10 - Parse") {
    assert(rawPoints.split("\n").toSeq.flatMap(parsePoint) == points)
  }

  val printedCoordinates: String =
    """#...#..###
      |#...#...#.
      |#...#...#.
      |#####...#.
      |#...#...#.
      |#...#...#.
      |#...#...#.
      |#...#..###""".stripMargin

  test("Day10 - Iterate and Print") {
    assert(printCoordinates(pointsStream(points)(3).map(_.position)) == printedCoordinates)
  }
}
