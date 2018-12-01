package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Sequences.{cumulative, cycle, findFirstDuplicate}

import scala.util.Try

object Day01 extends SimpleCommonPuzzle[Seq[Int], Int, Int] {
  override def parse(resource: String): Seq[Int] =
    readResource(resource).flatMap(row => Try(row.toInt).toOption)

  override def part1(frequencies: Seq[Int]): Int =
    frequencies.sum

  override def part2(frequencies: Seq[Int]): Int =
    findFirstDuplicate(cumulative[Int](0, _ + _)(cycle(frequencies))).get
}
