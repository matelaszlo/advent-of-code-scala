package com.lmat.adventofcode.year2018

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2018.Day18._
import com.lmat.adventofcode.year2018.Day18Definitions._

class Day18Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawState: String =
    """.#.#...|#.
      |.....#|##|
      |.|..|...#.
      |..|#.....#
      |#.#|||#|#|
      |...#.||...
      |.|....|...
      |||...#|.#|
      ||.||||..|.
      |...#.|..|.""".stripMargin

  val state: State = Map(
    (Coordinate(0,0),Ground), (Coordinate(1,0),LumberyYard), (Coordinate(2,0),Ground), (Coordinate(3,0),LumberyYard), (Coordinate(4,0),Ground), (Coordinate(5,0),Ground), (Coordinate(6,0),Ground), (Coordinate(7,0),Tree), (Coordinate(8,0),LumberyYard), (Coordinate(9,0),Ground),
    (Coordinate(0,1),Ground), (Coordinate(1,1),Ground), (Coordinate(2,1),Ground), (Coordinate(3,1),Ground), (Coordinate(4,1),Ground), (Coordinate(5,1),LumberyYard), (Coordinate(6,1),Tree), (Coordinate(7,1),LumberyYard), (Coordinate(8,1),LumberyYard), (Coordinate(9,1),Tree),
    (Coordinate(0,2),Ground), (Coordinate(1,2),Tree), (Coordinate(2,2),Ground), (Coordinate(3,2),Ground), (Coordinate(4,2),Tree), (Coordinate(5,2),Ground), (Coordinate(6,2),Ground), (Coordinate(7,2),Ground), (Coordinate(8,2),LumberyYard), (Coordinate(9,2),Ground),
    (Coordinate(0,3),Ground), (Coordinate(1,3),Ground), (Coordinate(2,3),Tree), (Coordinate(3,3),LumberyYard), (Coordinate(4,3),Ground), (Coordinate(5,3),Ground), (Coordinate(6,3),Ground), (Coordinate(7,3),Ground), (Coordinate(8,3),Ground), (Coordinate(9,3),LumberyYard),
    (Coordinate(0,4),LumberyYard), (Coordinate(1,4),Ground), (Coordinate(2,4),LumberyYard), (Coordinate(3,4),Tree), (Coordinate(4,4),Tree), (Coordinate(5,4),Tree), (Coordinate(6,4),LumberyYard), (Coordinate(7,4),Tree), (Coordinate(8,4),LumberyYard), (Coordinate(9,4),Tree),
    (Coordinate(0,5),Ground), (Coordinate(1,5),Ground), (Coordinate(2,5),Ground), (Coordinate(3,5),LumberyYard), (Coordinate(4,5),Ground), (Coordinate(5,5),Tree), (Coordinate(6,5),Tree), (Coordinate(7,5),Ground), (Coordinate(8,5),Ground), (Coordinate(9,5),Ground),
    (Coordinate(0,6),Ground), (Coordinate(1,6),Tree), (Coordinate(2,6),Ground), (Coordinate(3,6),Ground), (Coordinate(4,6),Ground), (Coordinate(5,6),Ground), (Coordinate(6,6),Tree), (Coordinate(7,6),Ground), (Coordinate(8,6),Ground), (Coordinate(9,6),Ground),
    (Coordinate(0,7),Tree), (Coordinate(1,7),Tree), (Coordinate(2,7),Ground), (Coordinate(3,7),Ground), (Coordinate(4,7),Ground), (Coordinate(5,7),LumberyYard), (Coordinate(6,7),Tree), (Coordinate(7,7),Ground), (Coordinate(8,7),LumberyYard), (Coordinate(9,7),Tree),
    (Coordinate(0,8),Tree), (Coordinate(1,8),Ground), (Coordinate(2,8),Tree), (Coordinate(3,8),Tree), (Coordinate(4,8),Tree), (Coordinate(5,8),Tree), (Coordinate(6,8),Ground), (Coordinate(7,8),Ground), (Coordinate(8,8),Tree), (Coordinate(9,8),Ground),
    (Coordinate(0,9),Ground), (Coordinate(1,9),Ground), (Coordinate(2,9),Ground), (Coordinate(3,9),LumberyYard), (Coordinate(4,9),Ground), (Coordinate(5,9),Tree), (Coordinate(6,9),Ground), (Coordinate(7,9),Ground), (Coordinate(8,9),Tree), (Coordinate(9,9),Ground))

  test("Parse"){
    assert(parse(rawState.split("\n")) == state)
  }

  test("Part1"){
    assert(part1(state) == 1147)
  }

  test("Part2"){
    // The test example turns to constant after minute 18 (holding only ground acres)
    assert(findCycle(state) == (18, 19))
  }
}
