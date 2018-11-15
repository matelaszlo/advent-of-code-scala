package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Maths.divisors

object Day20 extends SimpleCommonPuzzle[Int, Int, Int]{
  override def parse(resource: String): Int = Integer.parseInt(readResource(resource).head)

  /**
    * We can speed up the solution slightly by simplifying both sides by 10
    */
  override def part1(input: Int): Int =
    Stream.from(1).find(n => divisors(n).sum  >= (input / 10)).get

  override def part2(input: Int): Int =
    Stream.from(1).find(n => divisors(n).filter(_ * 50 >= n).map(_ * 11).sum  >= input).get
}
