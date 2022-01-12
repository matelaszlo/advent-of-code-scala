package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day07 extends SimpleCommonPuzzle[List[Int], Int, Int]{
  override def parse(resource: String): List[Int] =
    readResource(resource).flatMap(_.split(",").flatMap(_.toIntOption)).toList

  override def part1(crabs: List[Int]): Int = align(crabs, identity)._2
  override def part2(crabs: List[Int]): Int = align(crabs, n => n * (n + 1) / 2)._2

  // Find the alignment that takes the minimum amount of fuel based on step cost
  def align(crabs: List[Int], stepCost: Int => Int): (Int, Int) =
    (for {position <- crabs.min to crabs.max}
      yield (position, crabs.map(crab => Math.abs(position - crab)).map(stepCost).sum))
      .minBy { case (_, fuel) => fuel }
}
