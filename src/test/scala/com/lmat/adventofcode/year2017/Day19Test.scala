package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day19Definitions._
import com.lmat.adventofcode.year2017.Day19.{parseMap, part1, part2}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day19Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val rawMap =
    """
      |     |
      |     |  +--+
      |     A  |  C
      | F---|----E|--+
      |     |  |  |  D
      |     +B-+  +--+
    """.stripMargin

  val mapDefinitions = Map(
    (Position(0, 5),  VerticalPath),
    (Position(1, 5),  VerticalPath),
    (Position(1, 8),  Crossing),
    (Position(1, 9),  HorizontalPath),
    (Position(1, 10), HorizontalPath),
    (Position(1, 11), Crossing),
    (Position(2, 5),  CheckPoint('A')),
    (Position(2, 8),  VerticalPath),
    (Position(2, 11), CheckPoint('C')),
    (Position(3, 1),  CheckPoint('F')),
    (Position(3, 2),  HorizontalPath),
    (Position(3, 3),  HorizontalPath),
    (Position(3, 4),  HorizontalPath),
    (Position(3, 5),  VerticalPath),
    (Position(3, 6),  HorizontalPath),
    (Position(3, 7),  HorizontalPath),
    (Position(3, 8),  HorizontalPath),
    (Position(3, 9),  HorizontalPath),
    (Position(3, 10), CheckPoint('E')),
    (Position(3, 11), VerticalPath),
    (Position(3, 12), HorizontalPath),
    (Position(3, 13), HorizontalPath),
    (Position(3, 14), Crossing),
    (Position(4, 5),  VerticalPath),
    (Position(4, 8),  VerticalPath),
    (Position(4, 11), VerticalPath),
    (Position(4, 14), CheckPoint('D')),
    (Position(5, 5),  Crossing),
    (Position(5, 6),  CheckPoint('B')),
    (Position(5, 7),  HorizontalPath),
    (Position(5, 8),  Crossing),
    (Position(5, 11), Crossing),
    (Position(5, 12), HorizontalPath),
    (Position(5, 13), HorizontalPath),
    (Position(5, 14), Crossing)
  )

  test("Parse") {
    assert(parseMap(rawMap.drop(1).split("\n").map(_.toCharArray.toSeq)) == mapDefinitions)
  }

  test("Day19 - Part 1") {
    assert(part1(mapDefinitions) == "ABCDEF")
  }

  test("Day19 - Part 2") {
    assert(part2(mapDefinitions) == 38)
  }
}
