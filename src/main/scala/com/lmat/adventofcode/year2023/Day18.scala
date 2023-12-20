package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleMultiPuzzle
import com.lmat.adventofcode.year2023.Day18Definitions._
import com.lmat.util.Files.readResource

object Day18Definitions {
  case class Plan(direction: Direction, meters: Long)

  case class Coordinate(x: Long, y: Long)

  sealed trait Direction
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction
}
object Day18 extends SimpleMultiPuzzle[List[Plan], Long, List[Plan], Long]{
  override def parse1(resource: String): List[Plan] =
    readResource(resource).toList.flatMap(parsePlan)

  override def parse2(resource: String): List[Plan] =
    readResource(resource).toList.flatMap(parsePlan2)

  def parsePlan(row: String): Option[Plan] = {
    val pattern = s"(.) (\\d+) \\(#.*\\)".r

    row match {
      case pattern(dirRaw, meterRaw) =>
        for {
          direction <- parseDirection(dirRaw)
          meter <- meterRaw.toLongOption
        } yield Plan(direction, meter)
      case _ => None
    }
  }

  def parsePlan2(row: String): Option[Plan] = {
    val pattern = s". \\d+ \\(#(.*)(.)\\)".r

    row match {
      case pattern(rawDistance, rawDirection) =>
        for {
          direction <- parseDirection2(rawDirection)
          meter     <- BigInt(rawDistance, 16).toString(10).toLongOption
        } yield Plan(direction, meter)
      case _ => None
    }
  }

  def parseDirection(raw: String): Option[Direction] =
    raw match {
      case "U" => Some(North)
      case "R" => Some(East)
      case "L" => Some(West)
      case "D" => Some(South)
      case _ => None
    }

  def parseDirection2(raw: String): Option[Direction] =
    raw match {
      case "0" => Some(East)
      case "1" => Some(South)
      case "2" => Some(West)
      case "3" => Some(North)
      case _ => None
    }

  override def part1(plans: List[Plan]): Long = digLagoon(plans)

  override def part2(plans: List[Plan]): Long = digLagoon(plans)

  // Calculate polygon area using Shoelace's formula (https://en.wikipedia.org/wiki/Shoelace_formula)
  // While also adding in the side sections we dig out
  def digLagoon(plans: List[Plan]): Long = {
    val (_, area) = plans.foldLeft((Coordinate(0L, 0L), 1L)) { case ((coordinate, area), plan) =>
      val nextCoordinate = digSection(coordinate, plan)
      (nextCoordinate, area + coordinate.x * nextCoordinate.y - nextCoordinate.x * coordinate.y + plan.meters)
    }

    (Math.abs(area) / 2) + 1
  }

  def digSection(coordinate: Coordinate, plan: Plan): Coordinate = {
    plan.direction match {
      case East  => coordinate.copy(x = coordinate.x + plan.meters)
      case West  => coordinate.copy(x = coordinate.x - plan.meters)
      case South => coordinate.copy(y = coordinate.y + plan.meters)
      case North => coordinate.copy(y = coordinate.y - plan.meters)
    }
  }
}
