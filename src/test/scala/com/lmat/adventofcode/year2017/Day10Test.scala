package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day10._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day10Test extends FunSuite with TableDrivenPropertyChecks {

  test("Day 10 - Part 1") {
    val hash = knotHashRound(initialState(5), Seq(3, 4, 1, 5))
    val expectedState = State(Seq(3, 4, 2, 1, 0), 4, 4)

    assert(hash == expectedState)
    assert(hash.circularList.take(2).product == 12)
  }

  val lengths =
    Table(
      ("source",   "hash"),
      ("",         "a2582a3a0e66e6e86e3812dcb672a272"),
      ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd"),
      ("1,2,3",    "3efbe78a8d82f29979031a4aa0b16a9d"),
      ("1,2,4",    "63960835bcdc130f0b66d7ff4f6a5a8e"),
    )

  test("Day 10 - Part 2") {
    forAll(lengths) { (source, hash) =>
      assert(part2(calculateLengths(source)) == hash)
    }
  }

  test("Calculate Lengths") {
    assert(calculateLengths("1,2,3") == Seq(49, 44, 50, 44, 51, 17, 31, 73, 47, 23))
  }

  test("XOR") {
    assert(xor(Seq(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22)) == 64)
  }
}
