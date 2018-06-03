package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day11.{part1, part2}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day11Test extends FunSuite with TableDrivenPropertyChecks {

  val directions =
    Table(
      ("Directions",      "Distance", "Furthest"),
      ("ne,ne,ne",        3,          3),
      ("ne,ne,sw,sw",     0,          2),
      ("ne,ne,s,s",       2,          2),
      ("se,sw,se,sw,sw",  3,          3)
    )

  test("Day 11 - Part 1") {
    forAll(directions) { (direction, distance, _) =>
      assert(part1(direction.split(",")) == distance)
    }
  }

  test("Day 11 - Part 2") {
    forAll(directions) { (direction, _, furthest) =>
      assert(part2(direction.split(",")) == furthest)
    }
  }
}
