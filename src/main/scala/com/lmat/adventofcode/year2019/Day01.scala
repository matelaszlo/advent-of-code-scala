package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day01 extends SimpleCommonPuzzle[Seq[Int], Int, Int] {
  override def parse(resource: String): Seq[Int] =
    readResource(resource).flatMap(row => Try(row.toInt).toOption)

  override def part1(moduleMasses: Seq[Int]): Int =
    moduleMasses.map(fuel).sum

  override def part2(moduleMasses: Seq[Int]): Int =
    moduleMasses.map(fuel2).sum

  def fuel(mass: Int): Int = (mass / 3) - 2

  def fuel2(mass: Int): Int = {
    @tailrec
    def iterate(total: Int, current: Int): Int =
      if(current == 0) total
      else {
        val f = fuel(current) max 0
        iterate(total + f, f)
      }
    iterate(0, mass)
  }
}
