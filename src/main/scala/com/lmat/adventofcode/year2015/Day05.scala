package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day05 extends SimpleCommonPuzzle[Seq[String], Int, Int] {
  override def parse(resource: String): Seq[String] = readResource(resource)

  override def part1(strings: Seq[String]): Int = strings.count(nice)

  def nice(string: String): Boolean =
    countChars(string, Seq('a', 'e', 'i', 'o', 'u')) >= 3 &&
      hasDoubleLetter(string, 0) &&
      !containsAtLeastOne(string, Seq("ab", "cd", "pq", "xy"))

  def countChars(string: String, chars: Seq[Char]): Int =
    string.count(chars.contains)

  def hasDoubleLetter(string: String, skip: Int): Boolean =
    (string zip string.drop(skip + 1)).count{ case (a, b) => a == b } > 0

  def containsAtLeastOne(string: String, subStrings: Seq[String]): Boolean =
    subStrings.exists(sub => string.contains(sub))

  override def part2(strings: Seq[String]): Int = strings.count(nice2)

  def nice2(string: String): Boolean =
    hasDoubleTwice(string) && hasDoubleLetter(string, 1)

  def hasDoubleTwice(string: String): Boolean =
    string.toSeq.sliding(2).map(_.unwrap).zipWithIndex
      .exists { case (double, i) => string.updated(i, '\u0000').updated(i + 1, '\u0000').contains(double)}
}
