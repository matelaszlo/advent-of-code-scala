package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day17Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSet, SortedSet}

object Day17Definitions {
  type CityMap = Map[Coordinate, Int]

  case class Coordinate(x: Int, y: Int)

  object Coordinate {
    implicit val ordering: Ordering[Coordinate] = Ordering.by[Coordinate, (Int, Int)](c => (c.y, c.x))
  }

  sealed trait Direction
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction

}

object Day17 extends SimpleCommonPuzzle[CityMap, Int, Int] {
  override def parse(resource: String): CityMap =
    parseCityMap(readResource(resource).toList)

  def parseCityMap(rows: List[String]): CityMap =
    rows.zipWithIndex
      .flatMap { case (row, y) => row.toCharArray.toList.zipWithIndex
        .map { case (c, x) => (Coordinate(x, y), c.asDigit) }
      }
      .toMap

  def print(contraption: CityMap): String = {
    val maxX = contraption.keySet.map(_.x).max
    val maxY = contraption.keySet.map(_.y).max
    (0 to maxY)
      .map(y => (0 to maxX).map(x => contraption(Coordinate(x, y))).mkString("")).mkString("\n")
  }

  override def part1(cityMap: CityMap): Int = {
    val maxX = cityMap.keySet.map(_.x).max
    val maxY = cityMap.keySet.map(_.y).max
    findPath(cityMap, Coordinate(0, 0), Coordinate(maxX, maxY), (_, _, _, i2) => i2 <= 3, _ => true)
  }

  override def part2(cityMap: CityMap): Int = {
    val maxX = cityMap.keySet.map(_.x).max
    val maxY = cityMap.keySet.map(_.y).max
    findPath(cityMap, Coordinate(0, 0), Coordinate(maxX, maxY), (f, t, i, i2) => if (f == t) i2 <= 10 else i >= 4, _ >= 4)
  }

  def findPath(
    cityMap: CityMap, sC: Coordinate, eC: Coordinate, stepRules: (Direction, Direction, Int, Int) => Boolean,
    finishRules: Int => Boolean): Int = {
    def next(path: ((Coordinate, Direction, Int), Int)): Set[((Coordinate, Direction, Int), Int)] = {
      val ((c, d, i), h) = path
      head(c, d, i).filter { case (c2, d2, i2) => cityMap.contains(c2) && stepRules(d, d2, i, i2) }.map { case (c2, d2, i2) => ((c2, d2, i2), h + cityMap(c2)) }
    }

    def head(
      coordinate: Coordinate, direction: Direction, step: Int): Set[(Coordinate, Direction, Int)] = direction match {
      case North => Set((coordinate.copy(y = coordinate.y - 1), North, step + 1), (coordinate.copy(x = coordinate.x - 1), West, 1), (coordinate.copy(x = coordinate.x + 1), East, 1))
      case East => Set((coordinate.copy(x = coordinate.x + 1), East, step + 1), (coordinate.copy(y = coordinate.y - 1), North, 1), (coordinate.copy(y = coordinate.y + 1), South, 1))
      case South => Set((coordinate.copy(y = coordinate.y + 1), South, step + 1), (coordinate.copy(x = coordinate.x + 1), East, 1), (coordinate.copy(x = coordinate.x - 1), West, 1))
      case West => Set((coordinate.copy(x = coordinate.x - 1), West, step + 1), (coordinate.copy(y = coordinate.y + 1), South, 1), (coordinate.copy(y = coordinate.y - 1), North, 1))
    }

    @tailrec
    def loop(seen: Map[(Coordinate, Direction, Int), Int], paths: Set[((Coordinate, Direction, Int), Int)]): Int = {
      if (paths.isEmpty) {
        seen.filter { case ((c, _, i), _) => c == eC && finishRules(i) }.values.min
      }
      else {
        val nextPaths = paths.flatMap(next)
        val bestPaths = nextPaths.groupBy(_._1).map { case (k, v) => (k, v.minBy(_._2)._2) }.filter { case (c, heatLoss) => heatLoss <= seen.getOrElse(c, Int.MaxValue) }.toSet
        val updatedSeen = bestPaths.foldLeft(seen) { case (s, (c, p)) => s.updated(c, p) }

//        println(s"Seen: ${seen.size} Paths: ${paths.size} New Paths: ${bestPaths.size} Target Loss: ${seen.filter(_._1._1 == eC).values.minOption}")
        loop(updatedSeen, bestPaths)
      }
    }

    loop(Map.empty, Set(((sC, East, 0), 0), ((sC, South, 0), 0)))
  }
}
