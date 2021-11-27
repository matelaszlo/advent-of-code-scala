package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day01 extends SimpleCommonPuzzle[Array[Int], Long, Long] {
  override def parse(resource: String): Array[Int] =
    readResource(resource).flatMap(row => row.toIntOption).toArray

  // Ordering is N * Log(N)
  // Then we can find the answer in N
  override def part1(expenses: Array[Int]): Long =
    sumsUpTo(expenses.sorted)(2020).map { case (a, b) => a.toLong * b.toLong }.get

  // Ordering is N * Log(N)
  // Then we can find the answer in N^2
  // We can iterate through the array and for every element run our previously created function with (target - element)
  override def part2(expenses: Array[Int]): Long =
    sumsThreeUpTo(expenses.sorted)(2020).map { case (a, b, c) => a.toLong * b.toLong * c.toLong }.get

  // Invariant: The List is ordered
  // We solve it recursively with an Immutable Array
  def sumsUpTo(array: Array[Int])(target: Int): Option[(Int, Int)] = {
    @tailrec
    def loop(low: Int, high: Int): Option[(Int, Int)] =
      if (low >= high) None
      else {
        val lowValue = array(low)
        val highValue = array(high)
        //        println(s"$lowValue + $highValue")
        if ((lowValue + highValue) == target) Some(lowValue, highValue)
        else if ((lowValue + highValue) < target) loop(low + 1, high)
        else loop(low, high - 1)
      }
    loop(0, array.length - 1)
  }

  // Invariant: The List is ordered
  def sumsThreeUpTo(array: Array[Int])(target: Int): Option[(Int, Int, Int)] = {
    @tailrec
    def loop(first: Int): Option[(Int, Int, Int)] =
      if (first >= array.length) None
      else {
        val firstValue = array(first)
        val twoSum = sumsUpTo(array)(target - firstValue)
        if (twoSum.nonEmpty) {
          val (secondValue, thirdValue) = twoSum.get
          Some((firstValue, secondValue, thirdValue))
        }
        else loop(first + 1)
      }
    loop(0)
  }
}
