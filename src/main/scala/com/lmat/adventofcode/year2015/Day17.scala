package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.util.Files.readResource

import scala.math.Ordering

object Day17 extends CommonPuzzle[Seq[Int], Seq[Seq[Int]], Int, Int]{
  override def parse(resource: String): Seq[Int] = readResource(resource).map(_.toInt).toList

  override def preProcess(containers: Seq[Int]): Seq[Seq[Int]] =
    combinations(containers.sorted(Ordering[Int].reverse), 150)

  override def part1(combinations: Seq[Seq[Int]]): Int =
    combinations.size

  def combinations(containers: Seq[Int], target: Int): Seq[Seq[Int]] = {
    case class Branch(element: Int, remaining: Seq[Int], target: Int)

    val branches: Seq[Branch] =
      containers.scanRight(Seq[Int]())(_ +: _).filter(_.nonEmpty)
        .map { case h +: t => Branch(h, t, target - h) }
        .filter(b => b.element <= target)
        .filter(b => b.remaining.sum >= b.target)

    val (finished, unfinished) = branches.partition(_.target == 0)

    finished.map(b => Seq(b.element)) ++ unfinished.flatMap(branch => combinations(branch.remaining, branch.target).map(branch.element +: _))
  }

  override def part2(combinations: Seq[Seq[Int]]): Int =
    combinations
      .groupBy(_.size).view
      .mapValues(_.size)
      .minBy(_._1)
      ._2
}
