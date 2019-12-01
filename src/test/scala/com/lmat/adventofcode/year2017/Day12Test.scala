package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day12.{ProgramMap, connectedTo, groups, parseProgramMap}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day12Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val input =
    """0 <-> 2
      |1 <-> 1
      |2 <-> 0, 3, 4
      |3 <-> 2, 4
      |4 <-> 2, 3, 6
      |5 <-> 6
      |6 <-> 4, 5""".stripMargin

  val programMap: ProgramMap = Map(
    0 -> Set(2),
    1 -> Set(1),
    2 -> Set(0, 3, 4),
    3 -> Set(2, 4),
    5 -> Set(6),
    4 -> Set(2, 3, 6),
    6 -> Set(4, 5))

  test("Parse input") {
    assert(parseProgramMap(input.split("\n").toIndexedSeq) == programMap)
  }

  test("Day 12 - Part 1 - Connected to") {
    val result = connectedTo(programMap)(0)
    assert(result == Set(0, 5, 6, 2, 3, 4))
    assert(result.size == 6)
  }

  test("Day 12 - Part 2 - Groups") {
    val result = groups(programMap)
    assert(result == Set(Set(0, 5, 6, 2, 3, 4), Set(1)))
    assert(result.size == 2)
  }
}
