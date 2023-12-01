package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.year2023.Day01._
import org.scalatest.funsuite.AnyFunSuite

class Day01Test extends AnyFunSuite {
  val lines1 = List("1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet")
  val lines2 = List("two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen")
  test("part1") {
    assert(part1(lines1) == 142)
  }

  test("part2") {
    assert(part2(lines2) == 281)
  }
}
