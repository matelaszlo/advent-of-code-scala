package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day22._
import com.lmat.adventofcode.year2017.Day22Definitions._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day22Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawGrid =
    """..#
      |#..
      |...""".stripMargin

  val grid: Map[Position, String] = Map(
    Position(-1, 1)  -> "clean",    Position(0, 1)  -> "clean", Position(1, 1)  -> "infected",
    Position(-1, 0)  -> "infected", Position(0, 0)  -> "clean", Position(1, 0)  -> "clean",
    Position(-1, -1) -> "clean",    Position(0, -1) -> "clean", Position(1, -1) -> "clean",
  )

  test("Parse") {
    assert(parseMap(rawGrid.split("\n").toIndexedSeq.map(_.toIndexedSeq)) == grid)
  }

  val viruses =
    Table(
      ("evolved", "bursts", "count"),
      (false,     7,         5),
      (false,     70,        41),
      (false,     10000,     5587),
      (true,      100,       26)
    )

  forAll(viruses) { (evolved, bursts, count) =>
    test(s"$count infected bursts out of $bursts if the virus is evolved=$evolved") {
      val result =
        if (evolved) countInfectiousBursts(grid, decideOnDirectionV2, decideNodeStateV2, bursts)
        else countInfectiousBursts(grid, decideOnDirection, decideNodeState,  bursts)
      assert(count == result)
    }
  }
}
