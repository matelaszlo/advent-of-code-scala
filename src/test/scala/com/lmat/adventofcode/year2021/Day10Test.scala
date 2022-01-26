package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.year2021.Day10._
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {
  val lines =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin

  test("corrupted") {
    assert(corrupted("{([(<{}[<>[]}>{[]{[(<()>").contains('}'))
    assert(corrupted("[[<[([]))<([[{}[[()]]]").contains(')'))
    assert(corrupted("[{[{({}]{}}([{[{{{}}([]").contains(']'))
    assert(corrupted("[<(<(<(<{}))><([]([]()").contains(')'))
    assert(corrupted("<{([([[(<>()){}]>(<<{{").contains('>'))
  }

  test("autoComplete") {
    assert(autoComplete("[({(<(())[]>[[{[]{<()<>>") == "}}]])})]")
    assert(autoComplete("[(()[<>])]({[<{<<[]>>(")   == ")}>]})")
    assert(autoComplete("(((({<>}<{<{<>}{[]{[]{}")  == "}}>}>))))")
    assert(autoComplete("{<[[]]>}<{[{[{[]{()[[[]")  == "]]}}]}]}>")
    assert(autoComplete("<{([{{}}[<[[[<>{}]]]>[]]") == "])}>")
  }

  test("scoreIncomplete") {
    assert(scoreIncomplete("}}]])})]")  == 288957)
    assert(scoreIncomplete(")}>]})")    == 5566)
    assert(scoreIncomplete("}}>}>))))") == 1480781)
    assert(scoreIncomplete("]]}}]}]}>") == 995444)
    assert(scoreIncomplete("])}>")      == 294)
  }

  test("part1") {
    assert(part1(lines.split("\n").toList) == 26397)
  }

  test("part2") {
    assert(part2(lines.split("\n").toList) == 288957)
  }
}
