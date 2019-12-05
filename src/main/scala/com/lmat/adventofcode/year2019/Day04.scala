package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day04 extends SimpleCommonPuzzle[(Int, Int), Int, Int]{
  override def parse(resource: String): (Int, Int) =
    readResource(resource).headOption.flatMap(parseRange).get

  def parseRange(raw: String): Option[(Int, Int)] = {
    val pattern = s"(\\d+)-(\\d+)".r

    raw match {
      case pattern(from, to) => for {
        f <- Try(from.toInt).toOption
        t <- Try(to.toInt).toOption
      } yield (f, t)
      case _ => None
    }
  }

  override def part1(range: (Int, Int)): Int =
    solve(range, and(List(hasTwoAdjacent, monotonicallyIncreasing)))

  override def part2(range: (Int, Int)): Int =
    solve(range, and(List(hasTwoStandAloneAdjacent, monotonicallyIncreasing)))

  def solve(range: (Int, Int), isValid: List[Int] => Boolean): Int = {
    val (from, to) = range
    (from to to).map(toDigits).count(isValid)
  }

  def toDigits(n: Int): List[Int] =
    n.toString.toCharArray.map(_.asDigit).toList

  def and[A](fs: List[A => Boolean]): A => Boolean =
    fs.reduce[A => Boolean]{ case (f1, f2) => n => f1(n) && f2(n) }

  def or[A](fs: List[A => Boolean]): A => Boolean =
    fs.reduce[A => Boolean]{ case (f1, f2) => n => f1(n) || f2(n) }

  def hasTwoAdjacent(seq: List[Int]): Boolean =
    seq.sliding(2).exists { case d1 :: d2 :: _ => d1 == d2 }

  def hasTwoStandAloneAdjacent(seq: List[Int]): Boolean = {
    def beginning(seq: List[Int]): Boolean = seq match {
      case d1 :: d2 :: o1 :: _ => d1 == d2 && d1 != o1
      case _ => false
    }

    def end(seq: List[Int]): Boolean = beginning(seq.reverse)

    def middle(seq: List[Int]): Boolean =
      seq.sliding(4).exists { case o1 :: d1 :: d2 :: o2 :: _ => d1 == d2 && d1 != o1 && d1 != o2 }

    or[List[Int]](List(beginning, middle, end))(seq)
  }

  def monotonicallyIncreasing(seq: List[Int]): Boolean =
    seq.sliding(2).forall { case d1 :: d2 :: _ => d1 <= d2 }
}
