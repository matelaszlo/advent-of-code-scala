package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleMultiPuzzle
import com.lmat.adventofcode.year2023.Day06Definitions._
import com.lmat.util.Files.readResource

object Day06Definitions {
  case class Race(time: Long, distance: Long)
}

object Day06 extends SimpleMultiPuzzle[List[Race], Long, Race, Long] {
  override def parse1(resource: String): List[Race] =
    parseRaces(readResource(resource).toList)

  override def parse2(resource: String): Race =
    parseRace(readResource(resource).toList)

  def parseRaces(lines: List[String]): List[Race] = {
    val times = lines.head.split("[\\D]").flatMap(_.toLongOption)
    val distances = lines(1).split("[\\D]").flatMap(_.toLongOption)
    (times zip distances).map { case (t, d) => Race(t, d) }.toList
  }

  def parseRace(lines: List[String]): Race = {
    val time = lines.head.filter(_.isDigit).toLong
    val distance = lines(1).filter(_.isDigit).toLong
    Race(time, distance)
  }

  override def part1(races: List[Race]): Long =
    races.map(countRecords).product

  def countRecords(race: Race): Long =
    simulate(race.time).count {case (_, distance) => distance > race.distance}

  def simulate(time: Long): List[(Long, Long)] =
    (0L to time).map(hold => (hold, (time - hold) * hold)).toList

  override def part2(race: Race): Long = countRecordsFast(race)

  def countRecordsFast(race: Race): Long =
    race.time - LazyList.iterate(0L)(_ + 1L).takeWhile(hold => ((race.time - hold) * hold) <= race.distance).size * 2 + 1
}
