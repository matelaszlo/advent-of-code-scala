package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day13._
import com.lmat.adventofcode.year2023.Day13Definitions._
import com.lmat.util.Matrix
class Day13Test extends AnyFunSuite {
  val raw =
  """#.##..##.
    |..#.##.#.
    |##......#
    |##......#
    |..#.##.#.
    |..##..##.
    |#.#.##.#.
    |
    |#...##..#
    |#....#..#
    |..##..###
    |#####.##.
    |#####.##.
    |..##..###
    |#....#..#""".stripMargin

  val patterns = List(
    Matrix(
      Vector(
        Vector(Rock, Ash, Rock, Rock, Ash, Ash, Rock, Rock, Ash),
        Vector(Ash, Ash, Rock, Ash, Rock, Rock, Ash, Rock, Ash),
        Vector(Rock, Rock, Ash, Ash, Ash, Ash, Ash, Ash, Rock),
        Vector(Rock, Rock, Ash, Ash, Ash, Ash, Ash, Ash, Rock),
        Vector(Ash, Ash, Rock, Ash, Rock, Rock, Ash, Rock, Ash),
        Vector(Ash, Ash, Rock, Rock, Ash, Ash, Rock, Rock, Ash),
        Vector(Rock, Ash, Rock, Ash, Rock, Rock, Ash, Rock, Ash)
      )
    ),
    Matrix(
      Vector(
        Vector(Rock, Ash, Ash, Ash, Rock, Rock, Ash, Ash, Rock),
        Vector(Rock, Ash, Ash, Ash, Ash, Rock, Ash, Ash, Rock),
        Vector(Ash, Ash, Rock, Rock, Ash, Ash, Rock, Rock, Rock),
        Vector(Rock, Rock, Rock, Rock, Rock, Ash, Rock, Rock, Ash),
        Vector(Rock, Rock, Rock, Rock, Rock, Ash, Rock, Rock, Ash),
        Vector(Ash, Ash, Rock, Rock, Ash, Ash, Rock, Rock, Rock),
        Vector(Rock, Ash, Ash, Ash, Ash, Rock, Ash, Ash, Rock))
    )
  )

  test("parse") {
    assert(parsePatterns(raw.split("\n").toList) == patterns)
  }

  test("findMirrorRows") {
    assert(findMirrorRows(patterns.head.rows, None)    == (Vector.empty, 0, 1))
    assert(findMirrorRows(patterns.head.columns, None) ==
      (Vector(
        (Vector(Ash, Rock, Ash, Ash, Rock, Ash, Rock), Vector(Ash, Rock, Ash, Ash, Rock, Ash, Rock)),
        (Vector(Rock, Ash, Ash, Ash, Ash, Rock, Ash), Vector(Rock, Ash, Ash, Ash, Ash, Rock, Ash)),
        (Vector(Rock, Rock, Ash, Ash, Rock, Rock, Rock), Vector(Rock, Rock, Ash, Ash, Rock, Rock, Rock)),
        (Vector(Ash, Ash, Rock, Rock, Ash, Ash, Ash), Vector(Ash, Ash, Rock, Rock, Ash, Ash, Ash))
      ), 5, 6))

    assert(findMirrorRows(patterns(1).rows, None)      ==
      (Vector(
        (Vector(Rock, Rock, Rock, Rock, Rock, Ash, Rock, Rock, Ash), Vector(Rock, Rock, Rock, Rock, Rock, Ash, Rock, Rock, Ash)),
        (Vector(Ash, Ash, Rock, Rock, Ash, Ash, Rock, Rock, Rock), Vector(Ash, Ash, Rock, Rock, Ash, Ash, Rock, Rock, Rock)),
        (Vector(Rock, Ash, Ash, Ash, Ash, Rock, Ash, Ash, Rock), Vector(Rock, Ash, Ash, Ash, Ash, Rock, Ash, Ash, Rock))
      ), 4, 5))
      assert(findMirrorRows(patterns(1).columns, None)   == (Vector.empty, 0, 1))
  }

  test("part1") {
    assert(part1(patterns) == 405)
  }

  test("part2") {
    assert(part2(patterns) == 400)
  }
}
