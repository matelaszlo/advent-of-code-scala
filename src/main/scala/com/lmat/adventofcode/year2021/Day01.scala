package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day01 extends SimpleCommonPuzzle[Seq[Int], Int, Int] {
  override def parse(resource: String): Seq[Int] =
    readResource(resource).flatMap(_.toIntOption).toList

  override def part1(measurements: Seq[Int]): Int =
    countIfNext(measurements)(_ < _)

  override def part2(measurements: Seq[Int]): Int =
    countIfNext(windowedReduce(measurements)(3, _.sum))(_ < _)

  def countIfNext[A](seq: Seq[A])(f: (A, A) => Boolean): Int =
    seq.zip(seq.drop(1)).count(f.tupled)

  def windowedReduce[A](seq: Seq[A])(size: Int, f: Seq[A] => A): Seq[A] =
    seq.sliding(size).map(f).toSeq
}
