package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day12._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day12Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val books =
    Table(
      ("books",                   "sum"),
      ("""[1,2,3]""",              6),
      ("""{"a":2,"b":4}""",        6),
      ("""[[[3]]]""",              3),
      ("""{"a":{"b":4},"c":-1}""", 3),
      ("""{"a":[-1,1]}""",         0),
      ("""[-1,{"a":1}]""",         0),
      ("""[]""",                   0),
      ("""{}""",                   0)
    )

  test("Day12 - Part1") {
    forAll(books) { (book, sum) =>
      assert(part1(book) == sum)
    }
  }

  val books2 =
    Table(
      ("books",                               "sum"),
      ("""[1,2,3]""",                         6),
      ("""[1,{"c":"red","b":2},3]""",         4),
      ("""{"d":"red","e":[1,2,3,4],"f":5}""", 0),
      ("""[1,"red",5]""",                     6)
    )

  test("Day12 - Part2") {
    forAll(books2) { (book, sum) =>
      assert(part2(book) == sum)
    }
  }
}
