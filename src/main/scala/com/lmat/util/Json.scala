package com.lmat.util

import scala.annotation.tailrec
import scala.util.Try

/**
  * Naive Json implementation
  * - Arrays, Objects, String and Int fields
  * - No Whitespace
  * - No error reporting
  */
sealed trait Json
case class JsonObject(fields: Map[String, Json]) extends Json
case class JsonArray(elements: Seq[Json])        extends Json
case class JsonIntValue(value: Int)              extends Json
case class JsonStringValue(value:String)         extends Json

object Json {
  def parse(source:String): Option[Json] =
    Seq(parseJsonIntValue(source), parseJsonStringValue(source), parseJsonArray(source), parseJsonObject(source)).find(_.isDefined).flatten

  private def parseJsonObject(source: String): Option[JsonObject] = {
    def findElements(source: String): Seq[(String, String)] = splitWithCounting(source, ',')
      .map(segment => segment.splitAt(segment.indexOf(':')))
      .map(tuple => (tuple._1.drop(1).dropRight(1), tuple._2.drop(1)))

    val pattern = "\\{(.*)\\}".r
    source match {
      case pattern(inner) if inner.isEmpty => Some(JsonObject(Map()))
      case pattern(inner) =>
        val (keys, values) = findElements(inner).unzip
        Traverse
          .sequenceOption(values.map(parse))
          .map(vs => JsonObject((keys zip vs).toMap))
      case _ => None
    }
  }

  private def parseJsonArray(source:String): Option[JsonArray] = {
    val pattern = "\\[(.*)\\]".r
    source match {
      case pattern(inner) if inner.isEmpty => Some(JsonArray(Seq()))
      case pattern(inner) =>
        val elements = splitWithCounting(inner, ',').map(parse)
        Traverse
          .sequenceOption(elements)
          .map(JsonArray)
      case _ => None
    }
  }

  private def parseJsonIntValue(source: String): Option[JsonIntValue] =
    Try(source.toInt).toOption.map(JsonIntValue)

  private def parseJsonStringValue(source: String): Option[JsonStringValue] = {
    val pattern = "\"(.*)\"".r
    source match {
      case pattern(inner) => Some(JsonStringValue(inner))
      case _              => None
    }
  }

  private def splitWithCounting(source:String, by:Char): Seq[String] = {
    @tailrec
    def findSplitPoints(source:Seq[Char], points: Seq[Int], index: Int, count: Int): Seq[Int] = source match {
      case Seq() => points
      case c +: rest if c == '{' || c == '['   => findSplitPoints(rest, points,          index + 1, count + 1)
      case c +: rest if c == '}' || c == ']'   => findSplitPoints(rest, points,          index + 1, count - 1)
      case c +: rest if c == ',' && count == 0 => findSplitPoints(rest, points :+ index, index + 1, count)
      case _ +: rest                           => findSplitPoints(rest, points,          index + 1, count)
    }

    val points = findSplitPoints(source.toCharArray, Seq(), 0, 0)
    (-1 +: points).zip(points :+ source.length).map{
      case (from, to) => source.substring(from + 1, to)
    }
  }
}
