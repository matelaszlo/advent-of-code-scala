package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.year2019.Day03Definitions._
import com.lmat.adventofcode.year2019.Day03._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day03Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawWires0 = List(
    "R8,U5,L5,D3",
    "U7,R6,D4,L4")

  val rawWires1 = List(
    "R75,D30,R83,U83,L12,D49,R71,U7,L72",
    "U62,R66,U55,R34,D71,R55,D58,R83")

  val rawWires2 = List(
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

  val wires0 = List(
    List(Section(Right, 8), Section(Up, 5), Section(Left, 5), Section(Down, 3)),
    List(Section(Up, 7), Section(Right, 6), Section(Down, 4), Section(Left, 4)))

  val wires1 = List(
    List(Section(Right,75), Section(Down,30), Section(Right,83), Section(Up,83), Section(Left,12), Section(Down,49), Section(Right,71), Section(Up,7), Section(Left,72)),
    List(Section(Up,62), Section(Right,66), Section(Up,55), Section(Right,34), Section(Down,71), Section(Right,55), Section(Down,58), Section(Right,83)))

  val wires2 = List(
    List(Section(Right,98), Section(Up,47), Section(Right,26), Section(Down,63), Section(Right,33), Section(Up,87), Section(Left,62), Section(Down,20), Section(Right,33), Section(Up,53), Section(Right,51)),
    List(Section(Up,98), Section(Right,91), Section(Down,20), Section(Right,16), Section(Down,67), Section(Right,40), Section(Up,7), Section(Right,15), Section(Up,6), Section(Right,7)))

  val wireTable =
    Table(
      ("raw",     "wires", "manhattan", "steps"),
      (rawWires0, wires0,  6,           30),
      (rawWires1, wires1,  159,         610),
      (rawWires2, wires2,  135,         410),
    )

  test("Day 3 - Parse") {
    forAll(wireTable) { (raw, wires, _, _) =>
      assert(raw.flatMap(parseWire) == wires)
    }
  }

  test("Day 3 - Part 1") {
    forAll(wireTable) { (_, wires, result, _) =>
      assert(part1(preProcess(wires)) == result)
    }
  }

  test("Day 3 - Part 2") {
    forAll(wireTable) { (_, wires, _, result) =>
      assert(part2(preProcess(wires)) == result)
    }
  }
}
