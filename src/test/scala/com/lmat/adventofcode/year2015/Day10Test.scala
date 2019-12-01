package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day10.lookAndSay
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day10Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val sequence = Seq(
    "1",
    "11",
    "21",
    "1211",
    "111221",
    "312211"
  )

  test("Day10 - Look And Say") {
    val actual = LazyList.iterate("1")(lookAndSay)
    assert(actual.take(6) == sequence)
  }

}
