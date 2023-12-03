package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day03Definitions.{Tile, _}
import com.lmat.util.Files.readResource

object Day03Definitions {
  type Schematic = Map[Coordinate, Tile]

  case class Coordinate(x: Int, y: Int)

  object Coordinate {
    implicit val ordering: Ordering[Coordinate] = Ordering.by[Coordinate, (Int, Int)](c => (c.y, c.x))
  }
  sealed trait Tile
  case class Number(i: Int) extends Tile
  case class Symbol(s: Char) extends Tile
  case object Empty extends Tile

  case class Part(n: Int, coordinates: List[Coordinate])
  case class Gear(coordinate: Coordinate, parts: List[Int])
}

object Day03 extends SimpleCommonPuzzle[Schematic, Int, Int] {
  override def parse(resource: String): Schematic =
    parse(readResource(resource).toList)

  def parse(lines: List[String]): Schematic =
    lines.zipWithIndex.flatMap { case (line, y) => line.toCharArray.zipWithIndex.toList.map { case (c, x) => (Coordinate(x, y), parseTile(c)) } }.toMap

  def parseTile(char: Char): Tile = char match {
    case '.' => Empty
    case n if n.isDigit => Number(n.asDigit)
    case c => Symbol(c)
  }

  override def part1(schematic: Schematic): Int =
    collectParts(schematic).filter(isValid(schematic)).map(_.n).sum

  def collectParts(schematic: Schematic): List[Part] = {
    // Since the parts are ordered and we are folding left we know that numbers are built left to right
    def merge(parts: List[Part], component: (Int, Coordinate)): List[Part] =
      parts.find(_.coordinates.exists(isConsecutive(_, component._2)))
        .fold(Part(component._1, List(component._2)) :: parts)(p => Part(p.n * 10 + component._1, component._2 :: p.coordinates) :: parts.filterNot(_ == p))

    schematic.toList.collect { case (c, Number(n)) => (n, c) }.sortBy(_._2).foldLeft(List.empty[Part])(merge)
  }

  def isValid(schematic: Schematic)(part: Part): Boolean =
    part.coordinates.exists(c => neighbours(c).exists(n => isSymbol(schematic.getOrElse(n, Empty))))

  def neighbours(position: Coordinate): List[Coordinate] =
    (for {
      y <- -1 to 1
      x <- -1 to 1 if !(x == 0 && y == 0)
    } yield Coordinate(position.x + x, position.y + y)).toList

  def isConsecutive(c1: Coordinate, c2: Coordinate): Boolean =
    c1.y == c2.y && Math.abs(c1.x - c2.x) == 1

  def isSymbol(tile: Tile): Boolean = tile match {
    case _: Symbol => true
    case _ => false
  }


  override def part2(schematic: Schematic): Int = {
    collectGears(schematic).filter(isValid).map(_.parts.product).sum
  }

  def collectGears(schematic: Schematic): List[Gear] = {
    val parts = collectParts(schematic).filter(isValid(schematic))
    schematic.toList.collect { case (c, Symbol('*')) => c }.sorted
      .map(c => Gear(c, parts.filter(p => neighbours(c).exists(p.coordinates.contains)).map(_.n)))
  }

  def isValid(gear: Gear): Boolean =
    gear.parts.size == 2

}
