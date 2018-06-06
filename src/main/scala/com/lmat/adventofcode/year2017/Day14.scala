package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimplePuzzle
import com.lmat.adventofcode.year2017.Day10.{calculateLengths, knotHash}
import com.lmat.util.Files.readResource
import com.lmat.util.Strings.leftPad

import scala.annotation.tailrec

object Day14 extends SimplePuzzle[String, Int, Int] {
  override def parse(resource: String): String = readResource(resource).head

  val hashSize: Int           = 256
  val hashRounds: Int         = 64
  val hash: String => String  = input => knotHash(hashSize, hashRounds)(calculateLengths(input))
  val gridSize:Int            = 128

  override def part1(key: String): Int =
    calculateGrid(gridSize, hash)(key).map(_.count(_ == '1')).sum

  def calculateGrid(gridSize: Int, hash: String => String)(key: String): Seq[String] =
    for (row <- 0 until gridSize)
      yield hextoBin(hash(s"$key-$row"))

  def hextoBin(hex: String): String =
    leftPad(BigInt(hex, 16).toString(2))( '0', 128)

  override def part2(key: String): Int =
    countRegions(calculateGrid(gridSize, hash)(key))

  def countRegions(grid: Seq[String]): Int = {
    case class Position(row: Int, column: Int)

    val positions = for {
      row    <- grid.indices
      column <- 0 until grid.head.length if grid(row)(column) == '1'
    } yield Position(row, column)

    @tailrec
    def countRegions(positions: Seq[Position], acc: Int): Int = positions match {
      case Seq()     => acc
      case h +: tail => countRegions(removeNeighbours(tail, Seq(h)), acc + 1)
    }

    @tailrec
    def removeNeighbours(positions: Seq[Position], bases: Seq[Position]): Seq[Position] = {
      val (removed, rest) = positions.partition(p1 => bases.exists(p2 => areNeighbours(p1, p2)))
      if (removed.isEmpty) rest
      else removeNeighbours(rest, removed)
    }

    def areNeighbours(p1: Position, p2: Position): Boolean =
      Math.abs(p1.row - p2.row) + Math.abs(p1.column - p2.column) == 1

    countRegions(positions, 0)
  }
}
