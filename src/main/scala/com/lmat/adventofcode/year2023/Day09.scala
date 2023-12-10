package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day09 extends SimpleCommonPuzzle[List[List[Int]], Long, Long] {
  override def parse(resource: String): List[List[Int]] =
    readResource(resource).toList.map(parseRow)

  def parseRow(row: String): List[Int] =
    row.split(" ").flatMap(_.toIntOption).toList

  override def part1(oasis: List[List[Int]]): Long =
    oasis.map(expand).map(history).sum

  def expand(start: List[Int]): List[List[Int]] =
    LazyList.iterate(start)(prev => prev.zip(prev.drop(1)).map { case (a, b) => b - a }).takeWhile(_.exists(_ != 0)).toList

  def history(history: List[List[Int]]): Int =
    history.reverse.map(_.last).sum

  override def part2(oasis: List[List[Int]]): Long =
    oasis.map(expand).map(history2).sum

  def history2(history: List[List[Int]]): Int =
    history.reverse.map(_.head).foldLeft(0)((s, c) => c - s)
}
