package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Logic._

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
    solve(range, and(hasTwoAdjacent, monotonicallyIncreasing))

  override def part2(range: (Int, Int)): Int =
    solve(range, and(hasTwoStandAloneAdjacent, monotonicallyIncreasing))

  def solve(range: (Int, Int), isValid: List[Int] => Boolean): Int = {
    val (from, to) = range
    (from to to).map(toDigits).count(isValid)
  }

  def toDigits(n: Int): List[Int] =
    n.toString.toCharArray.map(_.asDigit).toList

  def hasTwoAdjacent(seq: List[Int]): Boolean =
    seq.sliding(2).exists {
      case List(d1, d2) => d1 == d2
      case _ => false
    }

  def hasTwoStandAloneAdjacent(seq: List[Int]): Boolean = {
    def beginning(seq: List[Int]): Boolean = seq match {
      case d1 :: d2 :: o1 :: _ => d1 == d2 && d1 != o1
      case _ => false
    }

    def end(seq: List[Int]): Boolean = beginning(seq.reverse)

    def middle(seq: List[Int]): Boolean =
      seq.sliding(4).exists {
        case List(o1, d1, d2, o2) => d1 == d2 && d1 != o1 && d1 != o2
        case _ => false
      }

    or[List[Int]](beginning, middle, end)(seq)
  }

  def monotonicallyIncreasing(seq: List[Int]): Boolean =
    seq.sliding(2).forall {
      case List(d1, d2) => d1 <= d2
      case _ => true
    }
}
