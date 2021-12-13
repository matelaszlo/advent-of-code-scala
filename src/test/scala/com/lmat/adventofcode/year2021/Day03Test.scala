package com.lmat.adventofcode.year2021

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2021.Day03._

class Day03Test extends AnyFunSuite {
  val diagnostics = List(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  test("gammaRate") {
    assert(gammaRate(diagnostics) == "10110")
  }

  test("epsilonRate") {
    assert(epsilonRate(diagnostics) == "01001")
  }

  test("powerConsumption") {
    assert(powerConsumption(diagnostics) == 198)
  }

  test("part1") {
    assert(part1(diagnostics) == 198)
  }

  test("oxygenGeneratorRating") {
    assert(oxygenGeneratorRating(diagnostics) == "10111")
  }

  test("co2ScrubberRating") {
    assert(co2ScrubberRating(diagnostics) == "01010")
  }

  test("lifeSupportRating") {
    assert(lifeSupportRating(diagnostics) == 230)
  }

  test("part2") {
    assert(part2(diagnostics) == 230)
  }
}
