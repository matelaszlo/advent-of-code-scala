package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day02 extends SimpleCommonPuzzle[Seq[Seq[Int]], Int, Int] {
  type SpreadSheet = Seq[Seq[Int]]

  override def parse(resource: String): SpreadSheet =
    parseSpreadsheet(readResource(resource))

  def parseSpreadsheet(lines: Seq[String]): SpreadSheet =
    lines.map(row =>
      row.split("\\s+").toSeq
        .map(_.toInt))

  override def part1(spreadsheet: SpreadSheet): Int =
    checksum(spreadsheet, largestDifference)

  override def part2(spreadsheet: SpreadSheet): Int =
    checksum(spreadsheet, evenlyDivisible)

  def checksum(data: SpreadSheet, rowCheckSum: Seq[Int] => Int): Int =
    data.map(rowCheckSum).sum

  def largestDifference(row: Seq[Int]): Int =
    row.max - row.min

  /**
    * Generate all pairs
    * Filter when i>=j to only compare one way
    * Get all the ones with 0 modulo
    * Sum and subtract the size for the checks with the item itself
    */
  def evenlyDivisible(row: Seq[Int]): Int = {
    val result = for {
      i <- row
      j <- row if i >= j && i % j == 0
    } yield i / j

    result.sum - row.size
  }
}
