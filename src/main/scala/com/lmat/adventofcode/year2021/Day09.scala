package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.math.Ordering.Int

object Day09 extends SimpleCommonPuzzle[Map[(Int, Int), Int], Int, Int] {
  override def parse(resource: String): Map[(Int, Int), Int] =
    parseHeightMap(readResource(resource))

  def parseHeightMap(rows: Seq[String]): Map[(Int, Int), Int] =
    rows.map(_.toCharArray.map(_.asDigit).zipWithIndex).zipWithIndex
      .flatMap { case (row, y) => row.map { case (v, x) => ((x, y), v) } }.toMap

  override def part1(heightMap: Map[(Int, Int), Int]): Int =
    lowPoints(heightMap).toList.map(_._3 + 1).sum

  override def part2(heightMap: Map[(Int, Int), Int]): Int =
    basins(heightMap).toList.map(_.size).sorted(Int.reverse).take(3).product

  def basins(heightMap: Map[(Int, Int), Int]): Set[Set[(Int, Int, Int)]] =
    lowPoints(heightMap).map(basin(heightMap))

  // Starting from the low points we can recursively calculate the basins
  // Since we collect a set of them we could even correctly handle the case when one basin contains multiple low points
  def basin(heightMap: Map[(Int, Int), Int])(lowPoint: (Int, Int, Int)): Set[(Int, Int, Int)] = {
    @tailrec
    def loop(basin: Set[(Int, Int, Int)], current: Set[(Int, Int, Int)]): Set[(Int, Int, Int)] =
      if (current.isEmpty) basin
      else {
        val newBasin = basin ++ current
        val newCurrent = (current.flatMap { case (x, y, _) => adjacent(heightMap)(x, y) } -- newBasin).filter { case (_, _, v) => v != 9 }
        loop(newBasin, newCurrent)
      }
    loop(Set(), Set(lowPoint))
  }

  def lowPoints(heightMap: Map[(Int, Int), Int]): Set[(Int, Int, Int)] =
    heightMap.collect { case ((x, y), v) if adjacent(heightMap)(x, y).forall(_._3 > v) => (x, y, v) }.toSet

  def adjacent(heightMap: Map[(Int, Int), Int])(x: Int, y: Int): Set[(Int, Int, Int)] =
    Set((0, -1), (-1, 0), (1, 0), (0, 1)).flatMap { case (xo, yo) => heightMap.get((x + xo, y + yo)).map(v => (x + xo, y + yo, v)) }
}
