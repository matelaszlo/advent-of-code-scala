package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day10Definitions._
import com.lmat.adventofcode.year2023.Day10._
class Day10Test extends AnyFunSuite {
  val raw1 =
    """-L|F7
      |7S-7|
      |L|7||
      |-L-J|
      |L|-JF""".stripMargin

  val raw2 =
    """7-F7-
      |.FJ|7
      |SJLL7
      ||F--J
      |LJ.LJ""".stripMargin

  val maze1 = Map(
    Coordinate(0, 0) -> Pipe(West, East),  Coordinate(1, 0) -> Pipe(North, East),  Coordinate(2, 0) -> Pipe(North, South), Coordinate(3, 0) -> Pipe(South, East),  Coordinate(4, 0) -> Pipe(South, West),
    Coordinate(0, 1) -> Pipe(South, West), Coordinate(1, 1) -> Start,              Coordinate(2, 1) -> Pipe(West, East),   Coordinate(3, 1) -> Pipe(South, West),  Coordinate(4, 1) -> Pipe(North, South),
    Coordinate(0, 2) -> Pipe(North, East), Coordinate(1, 2) -> Pipe(North, South), Coordinate(2, 2) -> Pipe(South, West),  Coordinate(3, 2) -> Pipe(North, South), Coordinate(4, 2) -> Pipe(North, South),
    Coordinate(0, 3) -> Pipe(West, East),  Coordinate(1, 3) -> Pipe(North, East),  Coordinate(2, 3) -> Pipe(West, East),   Coordinate(3, 3) -> Pipe(North, West),  Coordinate(4, 3) -> Pipe(North, South),
    Coordinate(0, 4) -> Pipe(North, East), Coordinate(1, 4) -> Pipe(North, South), Coordinate(2, 4) -> Pipe(West, East),   Coordinate(3, 4) -> Pipe(North, West),  Coordinate(4, 4) -> Pipe(South, East)
  )

  val maze2 = Map(
    Coordinate(0, 0) -> Pipe(South, West),  Coordinate(1, 0) -> Pipe(West, East),  Coordinate(2, 0) -> Pipe(South, East), Coordinate(3, 0) -> Pipe(South, West),  Coordinate(4, 0) -> Pipe(West, East),
    Coordinate(0, 1) -> Ground,             Coordinate(1, 1) -> Pipe(South, East), Coordinate(2, 1) -> Pipe(North, West), Coordinate(3, 1) -> Pipe(North, South), Coordinate(4, 1) -> Pipe(South, West),
    Coordinate(0, 2) -> Start,              Coordinate(1, 2) -> Pipe(North, West), Coordinate(2, 2) -> Pipe(North, East), Coordinate(3, 2) -> Pipe(North, East),  Coordinate(4, 2) -> Pipe(South, West),
    Coordinate(0, 3) -> Pipe(North, South), Coordinate(1, 3) -> Pipe(South, East), Coordinate(2, 3) -> Pipe(West, East),  Coordinate(3, 3) -> Pipe(West, East),   Coordinate(4, 3) -> Pipe(North, West),
    Coordinate(0, 4) -> Pipe(North, East),  Coordinate(1, 4) -> Pipe(North, West), Coordinate(2, 4) -> Ground,            Coordinate(3, 4) -> Pipe(North, East),  Coordinate(4, 4) -> Pipe(North, West)
  )

  test("parse") {
    assert(parseMaze(raw1.split("\n").toList) == maze1)
    assert(parseMaze(raw2.split("\n").toList) == maze2)
  }

  test("route") {
    val (startCoordinates, startPipe) = findStartPipe(maze2)
    val updated = maze2.updated(startCoordinates, startPipe)

    val rest = List(Coordinate(1, 2), Coordinate(1, 1), Coordinate(2, 1), Coordinate(2, 0), Coordinate(3, 0), Coordinate(3, 1), Coordinate(3, 2), Coordinate(4, 2), Coordinate(4, 3), Coordinate(3, 3), Coordinate(2, 3), Coordinate(1, 3), Coordinate(1, 4), Coordinate(0, 4), Coordinate(0, 3))

    assert(route(updated)(startCoordinates, startPipe.to) == startCoordinates :: rest)
    assert(route(updated)(startCoordinates, startPipe.from) == startCoordinates :: rest.reverse)
  }

  test("part1") {
    assert(part1(maze1) == 4)
    assert(part1(maze2) == 8)
  }

  val raw3 =
    """.F----7F7F7F7F-7....
      |.|F--7||||||||FJ....
      |.||.FJ||||||||L7....
      |FJL7L7LJLJ||LJ.L-7..
      |L--J.L7...LJS7F-7L7.
      |....F-J..F7FJ|L7L7L7
      |....L7.F7||L7|.L7L7|
      |.....|FJLJ|FJ|F7|.LJ
      |....FJL-7.||.||||...
      |....L---J.LJ.LJLJ...""".stripMargin

  val raw4 =
    """FF7FSF7F7F7F7F7F---7
      |L|LJ||||||||||||F--J
      |FL-7LJLJ||||||LJL-77
      |F--JF--7||LJLJ7F7FJ-
      |L---JF-JLJ.||-FJLJJ7
      ||F|F-JF---7F7-L7L|7|
      ||FFJF7L7F-JF7|JL---7
      |7-L-JL7||F7|L7F-7F7|
      |L.L7LFJ|||||FJL7||LJ
      |L7JLJL-JLJLJL--JLJ.L""".stripMargin

  val maze3 = parseMaze(raw3.split("\n").toList)
  val maze4 = parseMaze(raw4.split("\n").toList)

  test("part2") {
    assert(part2(maze1) == 1)
    assert(part2(maze2) == 1)
    assert(part2(maze3) == 8)
    assert(part2(maze4) == 10)
  }
}
