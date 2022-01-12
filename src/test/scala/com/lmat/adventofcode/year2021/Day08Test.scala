package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.year2021.Day08._
import org.scalatest.funsuite.AnyFunSuite

class Day08Test extends AnyFunSuite {

  val input =
    """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".stripMargin

  val shortInput = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

  test("deduction") {
    assert(deduction(parseLine(shortInput).get.inputs) == Map(
      Set('e', 'a', 'b', 'g', 'c', 'd') -> 0,
      Set('a', 'b') -> 1,
      Set('f', 'a', 'g', 'c', 'd') -> 2,
      Set('f', 'a', 'b', 'c', 'd') -> 3,
      Set('e', 'a', 'f', 'b') -> 4,
      Set('e', 'f', 'b', 'c', 'd') -> 5,
      Set('e', 'f', 'b', 'g', 'c', 'd') -> 6,
      Set('d', 'a', 'b') -> 7,
      Set('e', 'f', 'a', 'b', 'g', 'c', 'd') -> 8,
      Set('e', 'f', 'a', 'b', 'c', 'd') -> 9
    ))
  }

  test("part1") {
    assert(part1(input.split("\n").flatMap(parseLine).toList) == 26)
  }

  test("part2") {
    assert(part2(input.split("\n").flatMap(parseLine).toList) == 61229)
  }
}
