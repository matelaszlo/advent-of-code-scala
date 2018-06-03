package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.Puzzle
import com.lmat.util.Files.readResource

object Day13 extends Puzzle[Map[Int, Int], Int, Int] {
  override def parse(resource: String): Map[Int, Int] = parseLayers(readResource(resource))

  def parseLayers(rows: Seq[String]): Map[Int, Int] =
    rows.map(_.split(": ")).groupBy(_.head.toInt).mapValues(_.head(1).toInt)

  override def part1(layers: Map[Int, Int]): Int = calculateSeverity(layers, 0)

  def calculateSeverity(layers: Map[Int, Int], delay: Int): Int =
    layers.filter { case (layer, layerSize) => fallsOnZero(layer, layerSize, delay) }
      .map { case (layer, layerSize) => layer * layerSize }
      .sum

  def fallsOnZero(layer: Int, layerSize: Int, delay: Int): Boolean =
    (delay + layer) % ((layerSize - 1) * 2) == 0

  override def part2(layers: Map[Int, Int]): Int =
    Stream.from(0).find(delay => canEscape(layers, delay)).get

  def canEscape(layers: Map[Int, Int], delay: Int): Boolean =
    layers.forall { case (layer, layerSize) => !fallsOnZero(layer, layerSize, delay) }
}
