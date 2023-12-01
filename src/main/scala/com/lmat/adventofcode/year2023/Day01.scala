package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day01 extends SimpleCommonPuzzle[List[String], Int, Int] {
  override def parse(resource: String): List[String] =
    readResource(resource).toList

  override def part1(lines: List[String]): Int =
    calibrate(firstDigit, lastDigit)(lines)

  override def part2(lines: List[String]): Int =
    calibrate(realFirstDigit, realLastDigit)(lines)

  def calibrate(firstDigit: String => Int, lastDigit: String => Int)(lines: List[String]): Int =
    lines.map(line => firstDigit(line) * 10 + lastDigit(line)).sum

  def firstDigit(line: String): Int =
    line.find(_.isDigit).map(_.asDigit).getOrElse(0)

  def lastDigit(line: String): Int =
    firstDigit(line.reverse)

  def realFirstDigit(line: String): Int = {
    val (numeric, numericPosition) = firstNumericDigit(line).getOrElse(0, Int.MaxValue)
    val (spelled, spelledPosition) = firstSpelledDigit(line).getOrElse(0, Int.MaxValue)
    if (numericPosition < spelledPosition) numeric else spelled
  }

  def realLastDigit(line: String): Int = {
    val (numeric, numericPosition) = lastNumericDigit(line).getOrElse(0, Int.MinValue)
    val (spelled, spelledPosition) = lastSpelledDigit(line).getOrElse(0, Int.MinValue)
    if (numericPosition > spelledPosition) numeric else spelled
  }

  def firstNumericDigit(line: String): Option[(Int, Int)] =
    line.zipWithIndex
      .find { case (c, _) => c.isDigit }
      .map { case (c, i) => (c.asDigit, i) }

  def lastNumericDigit(line: String): Option[(Int, Int)] =
    line.zipWithIndex
      .findLast { case (c, _) => c.isDigit }
      .map { case (c, i) => (c.asDigit, i) }

  val digits: List[String] = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  def firstSpelledDigit(line: String): Option[(Int, Int)] =
    digits.map(d => (d, line.indexOf(d)))
      .filter { case (_, i) => i >= 0 }
      .map { case (d, i) => (digits.indexOf(d) + 1, i) }
      .minByOption { case (_, i) => i }

  def lastSpelledDigit(line: String): Option[(Int, Int)] =
    digits.map(d => (d, line.lastIndexOf(d)))
      .filter { case (_, i) => i >= 0 }
      .map { case (d, i) => (digits.indexOf(d) + 1, i) }
      .maxByOption { case (_, i) => i }
}
