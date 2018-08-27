package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day10 extends SimpleCommonPuzzle[String, Int, Int] {

  override def parse(resource: String): String = readResource(resource).head

  override def part1(digits: String): Int =
    Stream.iterate(digits)(lookAndSay).map(_.length).drop(40).head

  def toDigits(number:String): Seq[Int] =
    number.map(_.asDigit)

  @tailrec
  def lookAndSayLength(digits: String, n:Int): Int =
    if(n == 0) digits.length
    else lookAndSayLength(lookAndSay(digits), n - 1)

  def lookAndSay(digits: String): String =
    lookAndSay(toDigits(digits).toVector).mkString

  /**
    * As we are doing a large number of appends it is important to use Vector here
    * instead of String or List
    */
  def lookAndSay(digits: Vector[Int]): Vector[Int] = {
    val (acc, num, count) = digits.foldLeft((Vector[Int](), 0, 0)) {
      case ((acc, num, count), digit) if digit == num => (acc, num, count + 1)
      case ((acc, num, _), digit) if num == 0         => (acc, digit, 1)
      case ((acc, num, count), digit)                 => (acc :+ count :+ num, digit, 1)
    }
    acc :+ count :+ num
  }

  override def part2(digits: String): Int =
    Stream.iterate(digits)(lookAndSay).map(_.length).drop(50).head

}
