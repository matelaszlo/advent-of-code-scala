package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.Puzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Sequences.shiftRight

object Day01 extends Puzzle[String, Int, Int] {
  override def parse(resource: String): String =
    readResource(resource).head

  override def part1(captcha: String): Int =
    solveCaptcha(1)(captcha)

  override def part2(captcha: String): Int =
    solveCaptcha(captcha.length / 2)(captcha)

  def solveCaptcha(shiftAmount: Int)(captcha: String): Int = {
    val digits  = captcha.map(_.asDigit)
    val shifted = shiftRight(digits, shiftAmount)
    sumEqualDigits(digits, shifted)
  }

  def sumEqualDigits(digits: Seq[Int], digits2: Seq[Int]): Int =
    (digits zip digits2)
      .filter { case (a, b) => a == b }
      .map(_._1)
      .sum
}
