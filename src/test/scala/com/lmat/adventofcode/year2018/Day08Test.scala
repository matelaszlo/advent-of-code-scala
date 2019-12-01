package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day08Definitions._
import com.lmat.adventofcode.year2018.Day08._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day08Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val numbers = Seq(2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2)
  val tree =
    Node(Seq(
      Node(Seq(), Seq(10, 11, 12)),
      Node(Seq(
        Node(Seq(), Seq(99))),
        Seq(2))),
      Seq(1, 1, 2))

  test("Day08 - Build Tree") {
    assert(buildTree(numbers) == tree)
  }

  test("Day08 - Part 1") {
    assert(part1(tree) == 138)
  }

  test("Day08 - Part 2") {
    assert(part2(tree) == 66)
  }
}
