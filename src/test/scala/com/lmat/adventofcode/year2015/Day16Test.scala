package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day16._
import com.lmat.adventofcode.year2015.Day16Definitions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day16Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val rawAunts =
    """Sue 1: goldfish: 6, trees: 9, akitas: 0
      |Sue 2: goldfish: 7, trees: 1, akitas: 0
    """.stripMargin

  val aunts  = List(
    Aunt("Sue", 1, Map("goldfish" -> 6, "trees"-> 9, "akitas" -> 0)),
    Aunt("Sue", 2, Map("goldfish" -> 7, "trees"-> 1, "akitas" -> 0))
  )

  test("Day16 - Parse") {
    assert(rawAunts.split("\n").toSeq.flatMap(parseAunt) == aunts)
  }

}
