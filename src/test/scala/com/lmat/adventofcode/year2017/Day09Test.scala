package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day09.{part1, part2}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day09Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val streams =
    Table(
      ("stream",                       "score"),
      ("{}",                            1),
      ("{{{}}}",                        6),
      ("{{},{}}",                       5),
      ("{{{},{},{{}}}}",                16),
      ("{<a>,<a>,<a>,<a>}",             1),
      ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9),
      ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9),
      ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3),
    )

  test("Day09 - Part 1") {
    forAll(streams) { (stream, score) =>
      assert(part1(stream) == score)
    }
  }

  val garbage =
    Table(
      ("stream",              "garbage"),
      ("<>",                  0),
      ("<random characters>", 17),
      ("<<<<>",               3),
      ("<{!>}>",              2),
      ("<!!>",                0),
      ("<!!!>>",              0),
      ("""<{o"i!a,<{i<a>""", 10),
    )

  test("Day 09 - Part 2") {
    forAll(garbage) { (stream, garbage) =>
      assert(part2(stream) == garbage)
    }
  }
}
