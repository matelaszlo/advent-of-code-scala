package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day18._
import com.lmat.adventofcode.year2017.Day18Definitions._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day18Test extends FunSuite with TableDrivenPropertyChecks {

  val rawInstructions1 =
    """set a 1
      |add a 2
      |mul a a
      |mod a 5
      |snd a
      |set a 0
      |rcv a
      |jgz a -1
      |set a 1
      |jgz a -2
    """.stripMargin

  val instructions1 = Seq(
    Set("a","1"),
    Add("a","2"),
    Multiply("a","a"),
    Modulo("a","5"),
    Sound("a"),
    Set("a","0"),
    Recover("a"),
    Jump("a","-1"),
    Set("a","1"),
    Jump("a","-2")
  )

  test("Parse 1") {
    assert(rawInstructions1.split("\n").flatMap(row => parseInstruction(row)).toSeq == instructions1)
  }

  test("Day18 - Part 1") {
    assert(part1(instructions1) == 4)
  }

  val rawInstructions2 =
    """snd 1
      |snd 2
      |snd p
      |rcv a
      |rcv b
      |rcv c
      |rcv d
    """.stripMargin

  val instructions2 = Seq(
    Send("1"),
    Send("2"),
    Send("p"),
    Receive("a"),
    Receive("b"),
    Receive("c"),
    Receive("d")
  )

  test("Parse 2") {
    assert(rawInstructions2.split("\n").flatMap(row => parseInstruction(row, true)).toSeq == instructions2)
  }

  test("Day18 - Part 2") {
    assert(part2(instructions2) == 3)
  }
}
