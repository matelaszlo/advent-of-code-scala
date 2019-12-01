package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day21._
import com.lmat.adventofcode.year2017.Day21Definitions._
import com.lmat.util.Matrix
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day21Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val rawRules =
    """../.# => ##./#../...
      |.#./..#/### => #..#/..../..../#..#""".stripMargin

  val rules = Set(
    Rule(
      Matrix(Vector(Vector(false, false), Vector(false, true))),
      Matrix(Vector(Vector(true, true, false), Vector(true, false, false), Vector(false, false, false)))),
    Rule(
      Matrix(Vector(Vector(false, true, false), Vector(false, false, true), Vector(true, true, true))),
      Matrix(Vector(Vector(true, false, false, true), Vector(false, false, false, false), Vector(false, false, false, false), Vector(true, false, false, true))))
  )

  test("Parse Rules") {
    assert(rawRules.split("\n").map(parseRule).toSet == rules)
  }

  val expandedRules = Set(
    Rule(
      Matrix(Vector(Vector(false, false), Vector(false, true))),
      Matrix(Vector(Vector(true, true, false), Vector(true, false, false), Vector(false, false, false)))),
    Rule(
      Matrix(Vector(Vector(false, false), Vector(true, false))),
      Matrix(Vector(Vector(true, true, false), Vector(true, false, false), Vector(false, false, false)))),
    Rule(
      Matrix(Vector(Vector(false, true), Vector(false, false))),
      Matrix(Vector(Vector(true, true, false), Vector(true, false, false), Vector(false, false, false)))),
    Rule(
      Matrix(Vector(Vector(true, false), Vector(false, false))),
      Matrix(Vector(Vector(true, true, false), Vector(true, false, false), Vector(false, false, false)))),
    Rule(
      Matrix(Vector(Vector(true, true, true), Vector(false, false, true), Vector(false, true, false))),
      Matrix(Vector(Vector(true, false, false, true), Vector(false, false, false, false), Vector(false, false, false, false), Vector(true, false, false, true)))),
    Rule(
      Matrix(Vector(Vector(true, false, false), Vector(true, false, true), Vector(true, true, false))),
      Matrix(Vector(Vector(true, false, false, true), Vector(false, false, false, false), Vector(false, false, false, false), Vector(true, false, false, true)))),
    Rule(
      Matrix(Vector(Vector(true, true, true), Vector(true, false, false), Vector(false, true, false))),
      Matrix(Vector(Vector(true, false, false, true), Vector(false, false, false, false), Vector(false, false, false, false), Vector(true, false, false, true)))),
    Rule(
      Matrix(Vector(Vector(false, true, false), Vector(true, false, false), Vector(true, true, true))),
      Matrix(Vector(Vector(true, false, false, true), Vector(false, false, false, false), Vector(false, false, false, false), Vector(true, false, false, true)))),
    Rule(
      Matrix(Vector(Vector(false, true, false), Vector(false, false, true), Vector(true, true, true))),
      Matrix(Vector(Vector(true, false, false, true), Vector(false, false, false, false), Vector(false, false, false, false), Vector(true, false, false, true)))),
    Rule(
      Matrix(Vector(Vector(false, false, true), Vector(true, false, true), Vector(false, true, true))),
      Matrix(Vector(Vector(true, false, false, true), Vector(false, false, false, false), Vector(false, false, false, false), Vector(true, false, false, true)))),
    Rule(
      Matrix(Vector(Vector(true, true, false), Vector(true, false, true), Vector(true, false, false))),
      Matrix(Vector(Vector(true, false, false, true), Vector(false, false, false, false), Vector(false, false, false, false), Vector(true, false, false, true)))),
    Rule(
      Matrix(Vector(Vector(false, true, true), Vector(true, false, true), Vector(false, false, true))),
      Matrix(Vector(Vector(true, false, false, true), Vector(false, false, false, false), Vector(false, false, false, false), Vector(true, false, false, true))))
  )

  test("Expand Rules") {
    assert(rules.flatMap(expandRule) == expandedRules)
  }

  val grid = Matrix(Vector(
    Vector(true,  true,  false, true,  true,  false),
    Vector(true,  false, false, true,  false, false),
    Vector(false, false, false, false, false, false),
    Vector(true,  true,  false, true,  true,  false),
    Vector(true,  false, false, true,  false, false),
    Vector(false, false, false, false, false, false)))

  test("Apply Rules") {
    assert(applyRules(expandedRules, initialPattern, 2) == grid)
  }

  test("Pixels on") {
    assert(pixelsOn(grid) == 12)
  }
}
