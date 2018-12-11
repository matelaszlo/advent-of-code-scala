package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day11 extends SimpleCommonPuzzle[Int, String, String] {
  override def parse(resource: String): Int =
    readResource(resource).headOption.flatMap(r => Try(r.toInt).toOption).get

  override def part1(serialNumber: Int): String = {
    val gridSize = 300
    val grid = generateGrid(serialNumber)(gridSize)
    val (x, y) = findMaxTotalPower(grid, gridSize)(3)._1
    s"$x,$y"
  }

  def findMaxTotalPower(grid: Map[(Int, Int), Int], gridSize: Int)(n: Int): ((Int, Int), Int) =
    (for {
      x <- 0 to (gridSize - n)
      y <- 0 to (gridSize - n)
    } yield ((x, y), calculateTotalPower(grid, n)(x,y))).maxBy(_._2)

  def calculateTotalPower(grid: Map[(Int, Int), Int], n: Int)(x: Int, y: Int): Int =
    (for {
      xA <- x until x + n
      yA <- y until y + n
    } yield grid((xA, yA))).sum

  def generateGrid(serialNumber: Int)(n: Int): Map[(Int, Int), Int] =
    (for {
      x <- 0 until n
      y <- 0 until n
    } yield ((x, y), calculatePowerLevel(serialNumber)(x, y))).toMap

  def calculatePowerLevel(serialNumber: Int)(x: Int, y: Int): Int = {
    val rackId = x + 10
    hudredsDigit((rackId * y + serialNumber) * rackId) - 5
  }

  def hudredsDigit(number: Int): Int =
    number.toString.map(_.asDigit).takeRight(3).headOption.getOrElse(0)

  override def part2(serialNumber: Int): String = {
    val gridSize = 300
    val grid = generateGrid(serialNumber)(gridSize)

    // We are terminating early here as grids larger than 20x20 have negative values in them with a high chance
    val (x, y, i) = (1 to 20).map(i => {
      val ((x, y), power) = findMaxTotalPower(grid, gridSize)(i)
      ((x, y, i), power)
    }).maxBy(_._2)._1
    s"$x,$y,$i"
  }
}
