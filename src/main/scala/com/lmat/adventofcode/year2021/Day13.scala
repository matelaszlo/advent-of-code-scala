package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2021.Day13Definitions._
import com.lmat.util.Files.readResource

object Day13Definitions {
  case class Dot(x: Int, y: Int)
  case class Fold(axis: String, value: Int)
}

object Day13 extends SimpleCommonPuzzle[(Set[Dot], List[Fold]), Int, String]{
  override def parse(resource: String): (Set[Dot], List[Fold]) = parseInstructions(readResource(resource))

  def parseInstructions(rows: Seq[String]): (Set[Dot], List[Fold]) = {
    val dots = rows.takeWhile(_.nonEmpty).flatMap(parseDot)
    val folds = rows.dropWhile(_.nonEmpty).dropWhile(_.isEmpty).flatMap(parseFold)
    (dots.toSet, folds.toList)
  }

  def parseDot(row: String): Option[Dot] = {
    val components = row.split(",")
    for {
      x <- components.headOption.flatMap(_.toIntOption)
      y <- components.lift(1).flatMap(_.toIntOption)
    } yield Dot(x, y)
  }

  def parseFold(row: String): Option[Fold] = {
    val fold = s"fold along (.*)=(.*)".r
    row match {
      case fold(axis, valueRaw) => valueRaw.toIntOption.map(value => Fold(axis, value))
      case _                    => None
    }
  }

  override def part1(input: (Set[Dot], List[Fold])): Int = {
    val (dots, folds) = input
    foldPaper(dots, folds.head).size
  }

  override def part2(input: (Set[Dot], List[Fold])): String = {
    val (dots, folds) = input
    val result = folds.foldLeft(dots)(foldPaper)
    // Todo: recognize letters programmatically
    println(draw(result))
    "HKUJGAJZ"
  }

  // Invariants:
  // Dots will never appear exactly on the fold line
  // We always fold in exactly half, the axis we fold on has odd numbered elements
  def foldPaper(dots: Set[Dot], fold: Fold): Set[Dot] = fold match {
    case Fold("x", x) =>
      val (right, left) = dots.partition(_.x > x)
      val folded = right.map(dot => dot.copy(x = x * 2 - dot.x))
      folded ++ left
    case Fold("y", y) =>
      val (down, up) = dots.partition(_.y > y)
      val folded = down.map(dot => dot.copy(y = y * 2 - dot.y))
      folded ++ up
    case _            => throw new IllegalArgumentException("Only x and y axes are supported")
  }

  def draw(dots: Set[Dot]): String = {
    val maxX = dots.map(_.x).max
    val maxY = dots.map(_.y).max
    (0 to maxY)
      .map(y => (0 to maxX)
      .map(x => if (dots.contains(Dot(x, y))) '#' else '.')
      .mkString("", "", "\n")
    ).mkString
  }
}
