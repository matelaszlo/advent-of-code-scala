package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day13Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Sequences.shiftRight

object Day13Definitions {
  case class HappinessChange(subject: String, cause: String, change: Int)
}

object Day13 extends SimpleCommonPuzzle[Seq[HappinessChange], Int, Int]{
  override def parse(resource: String): Seq[HappinessChange] = readResource(resource).flatMap(parseHappinessChange)

  def parseHappinessChange(row: String): Option[HappinessChange] = {
    val happinessGain = "(.*) would gain (.*) happiness units by sitting next to (.*).".r
    val happinessLose = "(.*) would lose (.*) happiness units by sitting next to (.*).".r

    row match {
      case happinessGain(subject, change, cause) => Some(HappinessChange(subject, cause, change.toInt))
      case happinessLose(subject, change, cause) => Some(HappinessChange(subject, cause, - change.toInt))
      case _                                     => None
    }
  }

  override def part1(happinessChanges: Seq[HappinessChange]): Int = {
    val happinessChangeMap = buildHappinessChangeMap(happinessChanges)
    people(happinessChanges).permutations.map(calculateHappinessChange(happinessChangeMap)).max
  }

  def people(happinessChanges: Seq[HappinessChange]): Seq[String] =
    happinessChanges.map(_.subject).distinct

  def buildHappinessChangeMap(happinessChanges: Seq[HappinessChange]): Map[Set[String], Int] =
    happinessChanges.groupBy(h => Set(h.subject, h.cause)).view.mapValues(_.map(_.change).sum).toMap

  def calculateHappinessChange(happinessChangeMap: Map[Set[String], Int])(seating: Seq[String]): Int =
    (seating zip shiftRight(seating, 1)).map { case (h1, h2) => Set(h1, h2) }.map(happinessChangeMap(_)).sum

  override def part2(happinessChanges: Seq[HappinessChange]): Int = {
    val hostHappinessChanges = people(happinessChanges).flatMap(person => Seq(HappinessChange(person, "host", 0), HappinessChange("host", person, 0)))
    part1(happinessChanges ++ hostHappinessChanges)
  }
}
