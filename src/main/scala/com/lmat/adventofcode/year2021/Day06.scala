package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day06 extends SimpleCommonPuzzle[List[Int], Long, Long] {
  override def parse(resource: String): List[Int] =
    readResource(resource).flatMap(_.split(",").flatMap(_.toIntOption)).toList

  override def part1(fish: List[Int]): Long = simulateAt(7, 2)(fish, 80).values.sum
  override def part2(fish: List[Int]): Long = simulateAt(7, 2)(fish, 256).values.sum

  // Representing the lanternfish population by their count at each timer value is crucial for scaling
  def simulateAt(spawn: Int, extra: Int)(fish: List[Int], days: Int): Map[Int, Long] =
    LazyList.iterate(fish.groupBy(identity).view.mapValues(_.size.toLong).toMap)(next(spawn, extra)).take(days + 1).last

  def next(spawn: Int, extra: Int)(fish: Map[Int, Long]): Map[Int, Long] =
    fish.toList.map { case (timer, count) =>
      if (timer == 0) Map(spawn - 1 -> count, spawn + extra - 1 -> count)
      else Map(timer - 1 -> count)
    }.reduce(merge[Int])

  def merge[A](a: Map[A, Long], b: Map[A, Long]): Map[A, Long] =
    (a.keySet ++ b.keySet).map(key => (key, a.getOrElse(key, 0L) + b.getOrElse(key, 0L))).toMap
}
