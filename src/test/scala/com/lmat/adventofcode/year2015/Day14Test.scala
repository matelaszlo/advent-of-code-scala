package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day14._
import com.lmat.adventofcode.year2015.Day14Definitions.Reindeer
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day14Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawReindeers =
    """Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
      |Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
    """.stripMargin

  val reindeers = List(
    Reindeer("Comet",  14, 10, 127),
    Reindeer("Dancer", 16, 11, 162))

  test("Day14 - Parse") {
    assert(rawReindeers.split("\n").flatMap(parseReindeer).toSeq == reindeers)
  }

  val travelled =
    Table(
      ("reindeer", "valid"),
      ("hijklmmn", false),
      ("abbceffg", false),
      ("abbceabc", false),
      ("abcdffaa", true),
      ("ghjaabcc", true)
    )

  val reindeerMeasurements =
    Table(
      ("reindeer",                      "seconds", "distance"),
      (Reindeer("Dancer", 16, 11, 162), 1,         16),
      (Reindeer("Dancer", 16, 11, 162), 1000,      1056),
      (Reindeer("Comet",  14, 10, 127), 1,         14),
      (Reindeer("Comet",  14, 10, 127), 1000,      1120)
    )

  test("Day14 - Distance travelled") {
    forAll(reindeerMeasurements) { (reindeer, seconds, distance) =>
      assert(cumulativeTravelledStream(reindeer)(seconds) == distance)
    }
  }

  val reindeerPoints =
    Table(
      ("reindeer",                      "seconds", "distance"),
      (Reindeer("Dancer", 16, 11, 162), 1,         1),
      (Reindeer("Dancer", 16, 11, 162), 140,       139),
      (Reindeer("Dancer", 16, 11, 162), 1000,      689),
      (Reindeer("Comet",  14, 10, 127), 1,         0),
      (Reindeer("Comet",  14, 10, 127), 140,       1),
      (Reindeer("Comet",  14, 10, 127), 1000,      312)
    )

  test("Day14 - Points collected") {
    val points = cumulativePointsStream(reindeers)
    forAll(reindeerPoints) { (reindeer, seconds, distance) =>
      val seq = if(reindeer.name == "Comet") 0 else 1
      assert(points(seconds)(seq) == distance)
    }
  }
}
