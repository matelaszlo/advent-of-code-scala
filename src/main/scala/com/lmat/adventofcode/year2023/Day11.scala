package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day11Definitions._
import com.lmat.util.Files.readResource

object Day11Definitions {
  type Universe = Map[Coordinate, Tile]

  case class Coordinate(x: Int, y: Int)

  object Coordinate {
    implicit val ordering: Ordering[Coordinate] = Ordering.by[Coordinate, (Int, Int)](c => (c.y, c.x))
  }

  sealed trait Tile
  case object Galaxy extends Tile
  case object Space extends Tile
}

object Day11 extends SimpleCommonPuzzle[Universe, Long, Long] {
  override def parse(resource: String): Universe =
    parseUniverse(readResource(resource).toList)

  def parseUniverse(rows: List[String]): Universe =
    rows.zipWithIndex
      .flatMap { case (row, y) => row.toCharArray.toList.zipWithIndex
        .map { case (c, x) => (Coordinate(x, y), parseTile(c)) }
      }
      .toMap

  def parseTile(c: Char): Tile = c match {
    case '#' => Galaxy
    case _ => Space
  }

  override def part1(universe: Universe): Long =
    galaxyDistances(universe, 2)


  override def part2(universe: Universe): Long =
    galaxyDistances(universe, 1000000)

  def galaxyDistances(universe: Universe, n: Int): Long = {
    val galaxiesList = expand(galaxies(universe), emptyRows(universe), emptyColumns(universe), n)
    distances(galaxiesList).sum
  }

  def galaxies(universe: Universe): List[Coordinate] =
    universe.filter { case (_, tile) => tile == Galaxy }.toList.map(_._1).sorted

  def distances(galaxies: List[Coordinate]): List[Long] = {
    val combinations = galaxies.combinations(2).toList.map(list => (list.head, list(1)))
    combinations.map { case (a, b) => (Math.abs(a.y - b.y) + Math.abs(a.x - b.x)).toLong }
  }

  def emptyRows(universe: Universe): List[Int] =
    universe.groupBy(_._1.y).filter { case (_, v) => v.values.forall(_ == Space) }.keysIterator.toList.sorted

  def emptyColumns(universe: Universe): List[Int] =
    universe.groupBy(_._1.x).filter { case (_, v) => v.values.forall(_ == Space) }.keysIterator.toList.sorted

  def expand(galaxies: List[Coordinate], rows: List[Int], columns: List[Int], n: Int): List[Coordinate] = {
    val expandedRows = galaxies.map(c => c.copy(y = c.y + (rows.count(row => c.y > row) * (n - 1))))
    val expanded = expandedRows.map(c => c.copy(x = c.x + (columns.count(col => c.x > col) * (n - 1))))
    expanded
  }
}
