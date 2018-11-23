package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day25 extends SimpleCommonPuzzle[(Int, Int), Long, Unit]{
  override def parse(resource: String): (Int, Int) = readResource(resource).headOption.flatMap(parseIndex).get

  def parseIndex(row: String): Option[(Int, Int)] = {
    val pattern  = ".*row (.*), column (.*).".r

    row match {
      case pattern(r, c) => Try(r.toInt).flatMap(rI => Try(c.toInt).map(cI => (rI, cI))).toOption
      case _             => None
    }
  }

  lazy val codes: Stream[Long] =
    Stream.iterate(20151125L)(num => (num * 252533L) % 33554393L)

  override def part1(index: (Int, Int)): Long = {
    val (row, column) = index
    codes(findIndex(row, column) - 1)
  }

  /**
    * From the given row and column we can easily get the code index back
    * (Code indexes start from 1 as per the puzzle description)
    *
    * row, column => base => index
    * 1,   1      => 1    => 1
    * 2,   1      => 2    => 2
    * 1,   2      => 2    => 3
    * 3,   1      => 3    => 4
    * 2,   2      => 3    => 5
    * 1,   3      => 3    => 6
    */
  def findIndex(row: Int, column: Int): Int ={
    val base = row + column - 1
    (0 until base).sum + column
  }

  override def part2(index: (Int, Int)): Unit = ()
}
