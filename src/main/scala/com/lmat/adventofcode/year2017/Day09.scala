package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.Puzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day09 extends Puzzle[String, Int, Int] {
  override def parse(resource: String): String = readResource(resource).head

  /**
    * Every groups score is equal to its nesting level (starting from 1)
    * The level increases with every '{' and decreases with every '}'
    * We need to remove all garbage first to avoid counting invalid braces
    */
  override def part1(stream: String): Int = {
    @tailrec
    def calculateScore(remaining: List[Char], level: Int, acc: Int): Int = remaining match {
      case List()      => acc
      case '{' :: rest => calculateScore(rest, level + 1, acc + level)
      case '}' :: rest => calculateScore(rest, level - 1, acc)
      case _ :: rest   => calculateScore(rest, level, acc)
    }

    val simplified = removeGarbage(stream)
    calculateScore(simplified.toList, 1, 0)
  }

  override def part2(stream: String): Int =
    "<.*?>".r.findAllIn(removeCancelled(stream)).map(_.length - 2).sum

  def removeGarbage(stream: String): String =
    removeCancelled(stream).replaceAll("<.*?>", "")

  def removeCancelled(stream: String): String =
    stream.replaceAll("!.", "")
}
