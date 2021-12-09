package com.lmat.adventofcode.year2021

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2021.Day02._
import com.lmat.adventofcode.year2021.Day02Definitions._

class Day02Test extends AnyFunSuite {
  val rawCommands =
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin

  val commands = List(
    Forward(5),
    Down(5),
    Forward(8),
    Up(3),
    Down(8),
    Forward(2)
  )

  test("parse") {
    assert(rawCommands.split("\n").flatMap(parseCommand).toList == commands)
  }

  test("move1") {
    assert(commands.foldLeft(start)(move1) == Position(15, 10, 0))
  }

  test("part1") {
    assert(part1(commands) == 150)
  }

  test("move2") {
    assert(commands.foldLeft(start)(move2)== Position(15, 60, 10))
  }

  test("part2") {
    assert(part2(commands) == 900)
  }
}
