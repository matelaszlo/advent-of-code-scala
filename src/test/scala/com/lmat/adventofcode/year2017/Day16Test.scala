package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day16Definitions._
import com.lmat.adventofcode.year2017.Day16.{dance, parseMove}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day16Test extends FunSuite with TableDrivenPropertyChecks {

  val rawMoves: String = "s1,x3/4,pe/b"
  val moves: Seq[Move] = Seq(Spin(1), Exchange(3, 4), Partner('e', 'b'))

  test("Parse moves") {
    assert(rawMoves.split(",").toSeq.flatMap(parseMove) == moves)
  }

  test("Dance") {
    assert(dance("abcde", moves) == "baedc")
  }
}
