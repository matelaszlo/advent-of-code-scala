package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Sequences.countElements

object Day02 extends SimpleCommonPuzzle[Seq[String], Int, String] {
  override def parse(resource: String): Seq[String] = readResource(resource)

  override def part1(boxIds: Seq[String]): Int =
    boxIds.count(hasCount(2)) * boxIds.count(hasCount(3))

  def hasCount(n: Int)(boxId: String): Boolean =
    countElements(boxId).values.toSet.contains(n)

  override def part2(boxIds: Seq[String]): String =
    generatePairs(boxIds)
      .find{ case (b1, b2) => difference(b1, b2) == 1 }
      .map { case (b1, b2) => commonLetters(b1, b2)}.get

  def generatePairs(boxIds: Seq[String]): LazyList[(String, String)] = boxIds match {
    case h +: t => t.map((h, _)).to(LazyList) ++ generatePairs(t)
    case _      => LazyList()
  }

  def difference(b1: String, b2: String): Int =
    (b1 zip b2).count { case (c1, c2) => c1 != c2 }

  def commonLetters(b1: String, b2: String): String =
    (b1 zip b2).collect{ case (c1, c2) if c1 == c2 => c1 }.mkString
}
