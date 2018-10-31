package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day18Definitions._
import com.lmat.util.Files.readResource

object Day18Definitions {
  case class Grid(on: Set[(Int, Int)], size: Int) {
    def coordinates: Set[(Int, Int)] =
      (for {
        x <- 0 until size
        y <- 0 until size
      } yield (x, y)).toSet

    def isOn(x: Int, y: Int): Boolean = on.contains((x, y))

    def neighbours(x: Int, y: Int): Seq[Boolean] =
      neighbourCells(x, y).toSeq.map(on.contains)

    def neighbourCells(x: Int, y: Int): Set[(Int, Int)] =
      Set(
        (x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
        (x    , y - 1),             (x    , y + 1),
        (x + 1, y - 1), (x + 1, y), (x + 1, y + 1))
        .filter{ case (a, b) => a >= 0 && a < size && b >= 0 && b < size}

    def isCorner(x: Int, y: Int): Boolean = (x == 0 || x == size - 1) && (y == 0 || y == size - 1)
  }
}

object Day18 extends SimpleCommonPuzzle[Grid, Int, Int]{
  override def parse(resource: String): Grid =
    parseGrid(readResource(resource))

  def parseGrid(rows: Seq[String]): Grid = {
    val on = rows
      .zipWithIndex.flatMap{ case (row, x) => row.zipWithIndex.map{case (c, y) => (x, y, c)}}
      .filter{case (_, _, c) => c == '#'}
      .map{case (x, y, _) => (x, y)}
    Grid(on.toSet, rows.size)
  }

  override def part1(grid: Grid): Int =
    simulate(grid, 100, isOn1).on.size

  def simulate(start: Grid, steps: Int, isOn: (Grid, Int, Int) => Boolean): Grid =
    (0 until steps).foldLeft(start)((g, _) => next(isOn)(g))

  def next(isOn: (Grid, Int, Int) => Boolean)(grid: Grid): Grid =
    Grid(grid.coordinates.filter { case (x, y) => isOn(grid, x, y) }, grid.size)

  def isOn1(grid: Grid, x: Int, y: Int): Boolean = (grid.isOn(x, y), grid.neighbours(x, y).count(identity)) match {
    case (true,  count) if count == 2 || count == 3 => true
    case (false, count) if count == 3               => true
    case (_,     _)                                 => false
  }

  override def part2(grid: Grid): Int =
    simulate(grid, 100, isOn2).on.size

  def isOn2(grid: Grid, x: Int, y: Int): Boolean = (grid.isCorner(x, y), grid.isOn(x, y), grid.neighbours(x, y).count(identity)) match {
    case (true, _,     _)                                 => true
    case (_,    true,  count) if count == 2 || count == 3 => true
    case (_,    false, count) if count == 3               => true
    case (_,    _,     _)                                 => false
  }
}
