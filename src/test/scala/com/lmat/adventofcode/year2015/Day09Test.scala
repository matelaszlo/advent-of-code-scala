package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day09Definitions.Distance
import com.lmat.adventofcode.year2015.Day09._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day09Test extends FunSuite with TableDrivenPropertyChecks {

  val distances = Seq(
    Distance("London", "Dublin", 464),
    Distance("London", "Belfast", 518),
    Distance("Dublin", "Belfast", 141))

  test("Day09 - Part 1") {
    assert(part1(distances) == 605)
  }

  test("Day09 - Part 2") {
    assert(part2(distances) == 982)
  }
}
