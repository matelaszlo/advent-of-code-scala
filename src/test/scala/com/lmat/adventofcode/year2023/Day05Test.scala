package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day05Definitions._
import com.lmat.adventofcode.year2023.Day05._

class Day05Test extends AnyFunSuite {
  val raw =
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin

  val almanac = Almanac(List(79, 14, 55, 13),
    Map(
      "humidity-to-location" -> List(Conversion(56, 93, 4), Conversion(60, 56, 37)),
      "light-to-temperature" -> List(Conversion(68, 64, 13), Conversion(81, 45, 19), Conversion(45, 77, 23)),
      "temperature-to-humidity" -> List(Conversion(1, 0, 69), Conversion(0, 69, 1)),
      "fertilizer-to-water" -> List(Conversion(57, 7, 4), Conversion(42, 0, 7), Conversion(0, 11, 42), Conversion(49, 53, 8)),
      "soil-to-fertilizer" -> List(Conversion(39, 0, 15), Conversion(37, 52, 2), Conversion(0, 15, 37)),
      "water-to-light" -> List(Conversion(18, 25, 70), Conversion(88, 18, 7)),
      "seed-to-soil" -> List(Conversion(52, 50, 48), Conversion(50, 98, 2)))
  )

  test("parse") {
    assert(parseAlmanac(raw.split("\n").toList) == almanac)
  }

  test("part1") {
    assert(part1(almanac) == 35)
  }

  test("part2") {
    assert(part2(almanac) == 46)
  }

}
