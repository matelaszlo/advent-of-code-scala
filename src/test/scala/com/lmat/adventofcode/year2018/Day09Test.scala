package com.lmat.adventofcode.year2018

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2018.Day09Definitions._
import com.lmat.adventofcode.year2018.Day09._


class Day09Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val games =
    Table(
      ("gameDefinition",          "maxScore"),
      (GameDefinition(9,    25),         32),
      (GameDefinition(10, 1618),       8317),
      (GameDefinition(13, 7999),     146373),
      (GameDefinition(17, 1104),       2764),
      (GameDefinition(21, 6111),      54718),
      (GameDefinition(30, 5807),      37305)
    )

  test("Day09 - Play Game") {
    forAll(games) { (gameDefinition, maxScore) =>
      assert(part1(gameDefinition) == maxScore)
    }
  }
}
