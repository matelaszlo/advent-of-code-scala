package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day19Definitions._
import com.lmat.adventofcode.year2023.Day19._
class Day19Test extends AnyFunSuite {
  val raw =
    """px{a<2006:qkq,m>2090:A,rfg}
      |pv{a>1716:R,A}
      |lnx{m>1548:A,A}
      |rfg{s<537:gd,x>2440:R,A}
      |qs{s>3448:A,lnx}
      |qkq{x<1416:A,crn}
      |crn{x>2662:A,R}
      |in{s<1351:px,qqz}
      |qqz{s>2770:qs,m<1801:hdj,R}
      |gd{a>3333:R,R}
      |hdj{m>838:A,pv}
      |
      |{x=787,m=2655,a=1222,s=2876}
      |{x=1679,m=44,a=2067,s=496}
      |{x=2036,m=264,a=79,s=2244}
      |{x=2461,m=1339,a=466,s=291}
      |{x=2127,m=1623,a=2188,s=1013}""".stripMargin

  val workflows = Map(
    "px"  -> List(Check("a", LessThan, 2006, "qkq"),   Check("m", GreaterThan, 2090, "A"), Fallback("rfg")),
    "pv"  -> List(Check("a", GreaterThan, 1716, "R"),  Fallback("A")),
    "lnx" -> List(Check("m", GreaterThan, 1548, "A"),  Fallback("A")),
    "rfg" -> List(Check("s", LessThan, 537, "gd"),     Check("x", GreaterThan, 2440, "R"), Fallback("A")),
    "qs"  -> List(Check("s", GreaterThan, 3448, "A"),  Fallback("lnx")),
    "qkq" -> List(Check("x", LessThan, 1416, "A"),     Fallback("crn")),
    "crn" -> List(Check("x", GreaterThan, 2662, "A"),  Fallback("R")),
    "in"  -> List(Check("s", LessThan, 1351, "px"),    Fallback("qqz")),
    "qqz" -> List(Check("s", GreaterThan, 2770, "qs"), Check("m", LessThan, 1801, "hdj"), Fallback("R")),
    "gd"  -> List(Check("a", GreaterThan, 3333, "R"),  Fallback("R")),
    "hdj" -> List(Check("m", GreaterThan, 838, "A"),   Fallback("pv"))
  )

  val ratings = List(
    Map("x" -> 787,  "m" -> 2655, "a" -> 1222, "s" -> 2876),
    Map("x" -> 1679, "m" -> 44,   "a" -> 2067, "s" -> 496),
    Map("x" -> 2036, "m" -> 264,  "a" -> 79,   "s" -> 2244),
    Map("x" -> 2461, "m" -> 1339, "a" -> 466,  "s" -> 291),
    Map("x" -> 2127, "m" -> 1623, "a" -> 2188, "s" -> 1013)
  )

  test("parse"){
    val (parsedWorkflows, parsedRatings) = parseWorkflowsAndRatings(raw.split("\n").toList)
    assert(workflows == parsedWorkflows)
    assert(ratings == parsedRatings)
  }

  test("part1"){
    assert(part1(workflows, ratings) == 19114)
  }

  test("part2"){
    assert(part2(workflows, ratings) == 167409079868000L)
  }
}
