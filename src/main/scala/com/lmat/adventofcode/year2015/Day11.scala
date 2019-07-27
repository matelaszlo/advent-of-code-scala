package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day11 extends SimpleCommonPuzzle[String, String, String] {
  override def parse(resource: String): String = readResource(resource).head

  override def part1(current: String): String = nextPassword(current)

  def nextPassword(current:String): String =
    LazyList.iterate(current)(increment).drop(1).find(isValid).get

  def increment(password: String): String = password.toCharArray.toSeq match {
    case start :+ 'z'  => increment(start.mkString) :+ 'a'
    case start :+ last => (start :+ increment(last)).mkString
  }

  def increment(char: Char): Char =
    (char + 1).toChar

  def isValid(password:String): Boolean =
    runOfAtLeast(3)(password) && noneOf(Seq('i', 'o', 'l'))(password) && differentDoubles(2)(password)

  def runOfAtLeast(n: Int)(string: String): Boolean =
    string.sliding(n).exists(isRun)

  def isRun(string: String): Boolean =
    (string zip string.drop(1)).forall { case (l, r) => r - l == 1 }

  def noneOf(letters: Seq[Char])(string: String): Boolean =
    !letters.exists(string.contains(_))

  def differentDoubles(n:Int)(string: String): Boolean =
    (for (letter <- 'a' to 'z') yield Seq(letter, letter).mkString).count(string.contains(_)) >= n

  override def part2(current: String): String = nextPassword(nextPassword(current))
}
