package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2017.Day16Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Sequences.{shiftRight, swap}

object Day16Definitions {
  sealed trait Move
  case class Spin(value: Int) extends Move
  case class Exchange(posA: Int, posB: Int) extends Move
  case class Partner(progA: Char, progB: Char) extends Move
}

object Day16 extends SimpleCommonPuzzle[Seq[Move], String, String] {
  override def parse(resource: String): Seq[Move] =
    readResource(resource).head.split(",").flatMap(parseMove)

  def parseMove(source: String): Option[Move] = {
    val spinTest     = """s(.+)""".r
    val exchangeTest = """x(.+)/(.+)""".r
    val partnerTest  = """p(.+)/(.+)""".r

    source match {
      case spinTest(value)           => Some(Spin(value.toInt))
      case exchangeTest(posA, posB)  => Some(Exchange(posA.toInt, posB.toInt))
      case partnerTest(progA, progB) => Some(Partner(progA.head, progB.head))
      case _                         => None
    }
  }

  override def part1(moves: Seq[Move]): String = dance("abcdefghijklmnop", moves)

  def dance(start: String, moves: Seq[Move]): String =
    moves.foldLeft(start.toCharArray.toSeq)((pos, m) => move(pos, m)).mkString

  def move(positions: Seq[Char], move: Move): Seq[Char] = move match {
    case Spin(value)           => shiftRight(positions, value)
    case Exchange(posA, posB)  => swap(positions)(posA, posB)
    case Partner(progA, progB) => swap(positions)(positions.indexOf(progA), positions.indexOf(progB))
  }

  /**
    * Simulating 1 billion repetitions would be unacceptably slow here
    * Luckily the dance has a cycle so we can simplify it by finding the cycle size
    * And only simulating the dance a number of times equal to the remainder of the repetitions divided by the cycle size
    */
  override def part2(moves: Seq[Move]): String = {
    val start = "abcdefghijklmnop"
    val reps = simplify(start, moves, 1000000000)
    (0 until reps).foldLeft(start)((pos, _) => dance(pos, moves))
  }

  def simplify(start: String, moves:Seq[Move], repeats: Int): Int = {
    val cycleSize: Int = LazyList.iterate((start, 0)) { case (pos, i) => (dance(pos, moves), i + 1) }.drop(1).find(_._1 == start).map(_._2).getOrElse(repeats)
    repeats % cycleSize
  }
}
