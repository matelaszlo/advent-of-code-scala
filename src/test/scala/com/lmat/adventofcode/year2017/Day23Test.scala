package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day23.parseInstruction
import com.lmat.adventofcode.year2017.Day23Definitions._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day23Test extends FunSuite with TableDrivenPropertyChecks {
  val rawInstructions =
    """set b 99
      |set c b
      |jnz a 2
      |jnz 1 5
      |mul b 100
      |sub b -100000
      |set c b
      |sub c -17000
      |set f 1
      |set d 2
      |set e 2
      |set g d
      |mul g e
      |sub g b
      |jnz g 2
      |set f 0
      |sub e -1
      |set g e
      |sub g b
      |jnz g -8
      |sub d -1
      |set g d
      |sub g b
      |jnz g -13
      |jnz f 2
      |sub h -1
      |set g b
      |sub g c
      |jnz g 2
      |jnz 1 3
      |sub b -17
      |jnz 1 -23
    """.stripMargin

  val instructions = Seq(
    Set("b", "99"),
    Set("c", "b"),
    Jump("a", "2"),
    Jump("1", "5"),
    Multiply("b", "100"),
    Subtract("b", "-100000"),
    Set("c", "b"),
    Subtract("c", "-17000"),
    Set("f", "1"),
    Set("d", "2"),
    Set("e", "2"),
    Set("g", "d"),
    Multiply("g", "e"),
    Subtract("g", "b"),
    Jump("g", "2"),
    Set("f", "0"),
    Subtract("e", "-1"),
    Set("g", "e"),
    Subtract("g", "b"),
    Jump("g", "-8"),
    Subtract("d", "-1"),
    Set("g", "d"),
    Subtract("g", "b"),
    Jump("g", "-13"),
    Jump("f", "2"),
    Subtract("h", "-1"),
    Set("g", "b"),
    Subtract("g", "c"),
    Jump("g", "2"),
    Jump("1", "3"),
    Subtract("b", "-17"),
    Jump("1", "-23")
  )

  test("Parse") {
    assert(rawInstructions.split("\n").flatMap(row => parseInstruction(row)).toSeq == instructions)
  }
}
