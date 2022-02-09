package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day11 extends SimpleCommonPuzzle[Map[(Int, Int), Int], Int, Int] {
  override def parse(resource: String): Map[(Int, Int), Int] =
    parseMap(readResource(resource))

  def parseMap(rows: Seq[String]): Map[(Int, Int), Int] =
    rows.map(_.toCharArray.map(_.asDigit).zipWithIndex).zipWithIndex
      .flatMap { case (row, y) => row.map { case (v, x) => ((x, y), v) } }.toMap

  override def part1(energyMap: Map[(Int, Int), Int]): Int =
    simulate(energyMap).slice(1, 101).map(_.count(_._2 == 0)).sum

  override def part2(energyMap: Map[(Int, Int), Int]): Int =
    simulate(energyMap).zipWithIndex.find(_._1.values.forall(_ == 0)).map(_._2).getOrElse(0)

  def simulate(start: Map[(Int, Int), Int]): LazyList[Map[(Int, Int), Int]] =
    LazyList.iterate(start)(step)

  def step(energyMap: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    def flash(current: Map[(Int, Int), Int], x: Int, y: Int): Map[(Int, Int), Int] = {
      val flashed = current.updated((x, y), 0)
      val neighbours = adjacent(current)(x, y).filter(_._3 != 0)
      neighbours.foldLeft(flashed) { case (state, (xN, yN, vN)) => state.updated((xN, yN), vN + 1) }
    }

    @tailrec
    def loop(current: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
      val flashing = current.filter(_._2 > 9)
      if (flashing.isEmpty) current
      else loop(flashing.foldLeft(current) { case (state, ((x, y), _)) => flash(state, x, y) })
    }

    loop(energyMap.view.mapValues(_ + 1).toMap)
  }

  def adjacent(map: Map[(Int, Int), Int])(x: Int, y: Int): Set[(Int, Int, Int)] =
    Set((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)).flatMap { case (xo, yo) => map.get((x + xo, y + yo)).map(v => (x + xo, y + yo, v)) }
}
