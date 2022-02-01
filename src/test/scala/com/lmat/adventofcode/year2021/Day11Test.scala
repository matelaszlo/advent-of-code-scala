package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.year2021.Day11._
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {
  val small0 =
    """11111
      |19991
      |19191
      |19991
      |11111""".stripMargin

  val small1 =
    """34543
      |40004
      |50005
      |40004
      |34543""".stripMargin

  val small2 =
    """45654
      |51115
      |61116
      |51115
      |45654""".stripMargin

  val large0 =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin

  val large10 =
    """0481112976
      |0031112009
      |0041112504
      |0081111406
      |0099111306
      |0093511233
      |0442361130
      |5532252350
      |0532250600
      |0032240000""".stripMargin

  val large20 =
    """3936556452
      |5686556806
      |4496555690
      |4448655580
      |4456865570
      |5680086577
      |7000009896
      |0000000344
      |6000000364
      |4600009543""".stripMargin

  val large30 =
    """0643334118
      |4253334611
      |3374333458
      |2225333337
      |2229333338
      |2276733333
      |2754574565
      |5544458511
      |9444447111
      |7944446119""".stripMargin

  def parseMap(raw: String): Map[(Int, Int), Int] = Day11.parseMap(raw.split("\n"))

  test("simulate") {
    assert(simulate(parseMap(small0)).take(3).toList == List(parseMap(small0), parseMap(small1), parseMap(small2)))
    assert(simulate(parseMap(large0)).take(31).zipWithIndex.toList.filter(_._2 % 10 == 0).map(_._1) == List(parseMap(large0), parseMap(large10), parseMap(large20), parseMap(large30)))
  }

  test("part1") {
    assert(part1(parseMap(large0)) == 1656)
  }

  test("part2") {
    assert(part2(parseMap(large0)) == 195)
  }
}
