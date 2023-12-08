package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day05Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day05Definitions {
  case class Conversion(destStart: Long, sourceStart: Long, length: Long)

  case class Almanac(seeds: List[Long], conversions: Map[String, List[Conversion]])
}

object Day05 extends SimpleCommonPuzzle[Almanac, Long, Long] {
  override def parse(resource: String): Almanac =
    parseAlmanac(readResource(resource).toList)

  def parseAlmanac(rows: List[String]): Almanac = {
    val seeds = rows.head.drop(7).split(" ").flatMap(_.toLongOption).toList
    Almanac(seeds, parseConversions(rows.drop(2).appended("")))
  }

  def parseConversions(rows: List[String]): Map[String, List[Conversion]] = {
    @tailrec
    def parse(
      result: Map[String, List[Conversion]], currentName: String, currentConversions: List[Conversion],
      remaining: List[String]): Map[String, List[Conversion]] = {
      def parseConversion(numbers: String): Conversion = {
        val n = numbers.split(" ").flatMap(_.toLongOption)
        Conversion(n(0), n(1), n(2))
      }

      remaining match {
        case ::(head, rest) =>
          val pattern = s"(.*) map:".r
          head match {
            case pattern(title) => parse(result, title, currentConversions, rest)
            case "" => parse(result.updated(currentName, currentConversions), "", List.empty, rest)
            case numbers => parse(result, currentName, parseConversion(numbers) :: currentConversions, rest)
          }
        case Nil => result
      }
    }

    parse(Map.empty, "", List.empty, rows)
  }

  override def part1(almanac: Almanac): Long =
    almanac.seeds.map(seedToLocation(almanac)).min

  def seedToLocation(almanac: Almanac)(seed: Long): Long =
    List("seed-to-soil", "soil-to-fertilizer", "fertilizer-to-water", "water-to-light", "light-to-temperature", "temperature-to-humidity", "humidity-to-location")
      .foldLeft(seed)((num, step) => resolve(almanac.conversions(step))(num))

  def resolve(conversions: List[Conversion])(number: Long): Long =
    conversions.find(c => c.sourceStart <= number && c.sourceStart + c.length - 1 >= number).fold(number)(c => c.destStart + (number - c.sourceStart))

  override def part2(almanac: Almanac): Long = {
    val seedRanges = almanac.seeds.sliding(2, 2).map { case start :: length :: Nil => (start, start + length - 1) }.toList.sortBy(_._1)
    println(s"Seed Ranges: $seedRanges")
    val seed = LazyList.iterate(0L)(_ + 1L).map(locationToSeed(almanac)).dropWhile(s => !seedRanges.exists { case (start, end) => s >= start && s <= end }).head
    val location = seedToLocation(almanac)(seed)
    println(s"Seed: $seed Location:$location")
    location
  }

  def locationToSeed(almanac: Almanac)(seed: Long): Long =
    List("seed-to-soil", "soil-to-fertilizer", "fertilizer-to-water", "water-to-light", "light-to-temperature", "temperature-to-humidity", "humidity-to-location").reverse
      .foldLeft(seed)((num, step) => resolveInverse(almanac.conversions(step))(num))

  def resolveInverse(conversions: List[Conversion])(number: Long): Long =
    conversions.find(c => c.destStart <= number && c.destStart + c.length - 1 >= number).fold(number)(c => c.sourceStart + (number - c.destStart))
}