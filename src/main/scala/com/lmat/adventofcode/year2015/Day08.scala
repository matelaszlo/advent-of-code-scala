package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day08 extends SimpleCommonPuzzle[Seq[String], Int, Int] {

  override def parse(resource: String): Seq[String] = readResource(resource)

  override def part1(input: Seq[String]): Int = {
    input.map(_.length).sum - input.map(resolveEscapes).map(_.length).sum
  }

  def resolveEscapes(input:String): String = {
    def resolveHexEscape(input: String): String = {
      val hex = "(.*?)\\\\x([0-9,a-f]{2})(.*)".r
      input match {
        case hex(pre, num, post) => pre + Integer.parseInt(num, 16).toChar + resolveHexEscape(post)
        case _                   => input
      }
    }

    val simplified = input
      .replaceAll("\\\\\"", "\"")
      .replaceAll("\\\\\\\\", "\\\\")

    resolveHexEscape(simplified).drop(1).dropRight(1)
  }

  override def part2(input: Seq[String]): Int =
    input.map(addEscapes).map(_.length).sum - input.map(_.length).sum

  def addEscapes(input:String): String = {
    val escaped =
      input
        .replaceAll("\\\\","\\\\\\\\")
        .replaceAll("\"","\\\\\"")

    "\"" + escaped + "\""
  }
}
