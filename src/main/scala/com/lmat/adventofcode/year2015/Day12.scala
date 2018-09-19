package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util._

object Day12 extends SimpleCommonPuzzle[String, Int, Int] {
  override def parse(resource: String): String = readResource(resource).head

  override def part1(input: String): Int = extractAllNumbers(input).sum

  def extractAllNumbers(input: String): Seq[Int] =
    "[-0-9]+".r.findAllIn(input).toSeq.map(_.toInt)

  override def part2(input: String): Int =
    sumAllInts(Json.parse(input).get)

  def sumAllInts(json: Json): Int = json match {
    case JsonIntValue(value) => value
    case JsonStringValue(_)  => 0
    case JsonArray(elements) => elements.map(sumAllInts).sum
    case JsonObject(fields)  => if(fields.values.exists(isRed)) 0 else fields.values.map(sumAllInts).sum
  }

  def isRed(json: Json): Boolean = json match {
    case JsonStringValue(value) => value == "red"
    case _                      => false
  }
}
