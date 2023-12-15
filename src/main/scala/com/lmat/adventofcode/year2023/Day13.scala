package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day13Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix

import scala.annotation.tailrec

object Day13Definitions {
  sealed trait Tile
  case object Ash extends Tile
  case object Rock extends Tile
}

object Day13 extends SimpleCommonPuzzle[List[Matrix[Tile]], Int, Int] {
  override def parse(resource: String): List[Matrix[Tile]] =
    parsePatterns(readResource(resource).toList)

  def parsePatterns(rows: List[String]): List[Matrix[Tile]] = {
    @tailrec
    def loop(collected: List[Matrix[Tile]], current: List[String], remaining: List[String]): List[Matrix[Tile]] =
      remaining match {
        case row :: rest if row.isEmpty => loop(parsePattern(current.reverse) :: collected, List.empty, rest)
        case row :: rest => loop(collected, row :: current, rest)
        case Nil => (parsePattern(current.reverse) :: collected).reverse
      }

    loop(List.empty, List.empty, rows)
  }

  def parsePattern(rows: List[String]): Matrix[Tile] =
    Matrix(rows.map(_.toCharArray.map(parseTile).toVector).toVector)

  def parseTile(c: Char): Tile = c match {
    case '#' => Rock
    case _ => Ash
  }

  def print(pattern: Matrix[Tile]): String =
    pattern.rows.map(_.map {
      case Ash => '.'
      case Rock => '#'
    }.mkString("")).mkString("\n")

  def update(matrix: Matrix[Tile], y: Int, x: Int)(value: Tile): Matrix[Tile] =
    Matrix(matrix.rows.updated(y, matrix.rows(y).updated(x, value)))

  def inverse(tile: Tile): Tile = tile match {
    case Ash => Rock
    case Rock => Ash
  }

  override def part1(patterns: List[Matrix[Tile]]): Int =
    patterns.map(p => mirrorSummary(p, None, None)).sum

  override def part2(patterns: List[Matrix[Tile]]): Int =
    patterns.map(findSmudgedSummary).sum

  // Note that we have to exclude previous patterns even when they are still valid with the smudge
  def findSmudgedSummary(pattern: Matrix[Tile]): Int = {
    val rowSize = pattern.rows.head.size
    val (rows, y, _) = findMirrorRows(pattern.rows, None)
    val (_, x, _) = findMirrorRows(pattern.columns, None)
    val excludeY = if (rows.nonEmpty) Some(y) else None
    val excludeX = if (rows.isEmpty) Some(x) else None

    smudge(pattern, rowSize).map(s => mirrorSummary(s, excludeY, excludeX)).find(_ != 0).getOrElse(0)
  }

  def smudge(pattern: Matrix[Tile], rowSize: Int): LazyList[Matrix[Tile]] = {
    LazyList.iterate(0)(_ + 1).map(i => {
      val y = i / rowSize
      val x = i % rowSize
      val tile = inverse(pattern.rows(y)(x))
      update(pattern, y, x)(tile)
    })
  }

  def mirrorSummary(pattern: Matrix[Tile], excludeY: Option[Int], excludeX: Option[Int]): Int = {
    val (rows, y, _) = findMirrorRows(pattern.rows, excludeY)
    if (rows.nonEmpty) y * 100
    else {
      val (_, x, _) = findMirrorRows(pattern.columns, excludeX)
      x
    }
  }

  def findMirrorRows(
    rows: Vector[Vector[Tile]], exclude: Option[Int]): (Vector[(Vector[Tile], Vector[Tile])], Int, Int) = {
    val (pairs, x) = (1 until rows.size)
      .map(i => (rows.take(i).reverse, rows.drop(i), i))
      .map { case (a, b, i) => (a zip b, i) }
      .find { case (z, i) => !exclude.contains(i) && z.forall { case (x, y) => x == y } }
      .getOrElse((Vector.empty, 0))
    (pairs, x, x + 1)
  }
}
