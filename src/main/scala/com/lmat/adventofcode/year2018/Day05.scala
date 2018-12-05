package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day05 extends CommonPuzzle[String, String, Int, Int] {
  override def parse(resource: String): String = readResource(resource).head

  /**
    * We can use the initial reduction step for the start of both parts to avoid duplicating work
    */
  override def preProcess(polymer: String): String = react(polymer)

  def react(polymer: String): String = {
    @tailrec
    def reactOnce(polymer: Vector[Char], built: Vector[Char]): Vector[Char] = polymer match {
      case p1 +: p2 +: rest if areReacting(p1, p2) => reactOnce(rest, built)
      case p1 +: p2 +: rest                        => reactOnce(p2 +: rest, built :+ p1)
      case p                                       => built ++ p
    }

    @tailrec
    def iterate(polymer: Vector[Char]): Vector[Char] = {
      val next = reactOnce(polymer, Vector())
      if(next == polymer) polymer
      else iterate(next)
    }

    iterate(polymer.toCharArray.toVector).mkString
  }

  def areReacting(p1: Char, p2: Char): Boolean =
    p1 != p2 && p1.toLower == p2.toLower

  override def part1(reduced: String): Int =
    reduced.length

  /**
    * We can start from the reduced size polymer that we calculated in part 1 to speed up the process
    * Only check for letters that appear in the reduced size polymer as opposed to the whole abc
    */
  override def part2(reduced: String): Int =
    unitTypes(reduced).map(removeAll(reduced, _)).map(react).map(_.length).min

  def unitTypes(polymer: String): Set[Char] =
    polymer.toCharArray.map(_.toLower).toSet

  def removeAll(polymer: String, unit: Char): String =
    polymer.filterNot(c => c == unit || c == unit.toUpper)
}
