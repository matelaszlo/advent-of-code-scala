package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day04 extends SimpleCommonPuzzle[String, Int, Int]{
  override def parse(resource: String): String = readResource(resource).head

  override def part1(key: String): Int =
    findFirstHashStartingWith(key, "00000")

  override def part2(key: String): Int =
    findFirstHashStartingWith(key, "000000")

  def findFirstHashStartingWith(key: String, pattern: String): Int =
    LazyList.from(1).map(i => (i, md5(s"$key$i")))
      .find { case (_, hash) => hash.startsWith(pattern) }
      .map(_._1).get

  def md5(text: String) : String =
    java.security.MessageDigest.getInstance("MD5")
      .digest(text.getBytes())
      .map { "%02x".format(_) }
      .mkString
}
