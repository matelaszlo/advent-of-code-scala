package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day14Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix
import com.lmat.util.Matrix.{flipHorizontal, rotateLeft, rotateRight}

object Day14Definitions {
  type Dish = Matrix[Tile]

  sealed trait Tile
  case object Round extends Tile
  case object Cube extends Tile
  case object Empty extends Tile

  sealed trait Direction
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction
}

object Day14 extends SimpleCommonPuzzle[Dish, Int, Int] {
  override def parse(resource: String): Dish =
    parseDish(readResource(resource).toList)

  def parseDish(rows: List[String]): Dish =
    Matrix(rows.map(_.toCharArray.map(parseTile).toVector).toVector)

  def parseTile(c: Char): Tile = c match {
    case '#' => Cube
    case 'O' => Round
    case _ => Empty
  }

  def print(dish: Dish): String =
    dish.rows.map(_.map {
      case Round => 'O'
      case Cube => '#'
      case Empty => '.'
    }.mkString("")).mkString("\n")

  override def part1(dish: Dish): Int =
    loadDish(tilt(dish, North))

  override def part2(dish: Dish): Int = {
    val n = 1000000000

    def findCycle: (Int, Int) = {
      val states = LazyList.iterate(dish)(cycle).scanLeft[List[Dish]](List())((all, s) => s :: all).dropWhile(l => l.size == l.distinct.size).head
      val target = states.head
      (states.reverse.indexOf(target), states.size - 1)
    }

    val (start, end) = findCycle
    val size = end - start
    val offset = (n - start) % size
    val element = start + offset

    println(s"Cycle starts at $start and ends at $end and has a size off $size elements")
    println(s"The ${n}th element will match with the ${element}th because it has the same offset in the cycle: $offset")

    val result = LazyList.iterate(dish)(cycle).drop(element).head
    loadDish(result)
  }

  def loadDish(dish: Dish): Int = {
    val columns = dish.columns
    val maxX = columns.head.size
    columns.map(_.zipWithIndex.map { case (t, i) => if (t == Round) maxX - i else 0 }.sum).sum
  }

  def cycle(dish: Dish): Dish = {
    val directions: List[Direction] = List(North, West, South, East)
    directions.foldLeft(dish)((a, d) => tilt(a, d))
  }

  def tilt(dish: Matrix[Tile], direction: Direction): Matrix[Tile] = {
    direction match {
      case North => rotateRight(tilt(rotateLeft(dish), West))
      case East => flipHorizontal(tilt(flipHorizontal(dish), West))
      case South => rotateLeft(tilt(rotateRight(dish), West))
      case West =>
        val rows = dish.rows.map(row => {
          val added = row.appended(Cube) // We insert a Cube at the end to make the fold easier
          added.foldLeft((Vector.empty[Tile], Vector.empty[Tile], Vector.empty[Tile])) { case ((finished, empties, rounds), current) => current match {
            case Round => (finished, empties, rounds :+ current)
            case Empty => (finished, empties :+ current, rounds)
            case Cube => (finished ++ rounds ++ empties :+ current, Vector.empty, Vector.empty)
          }
          }._1.dropRight(1) // We drop the inserted Cube
        })
        Matrix(rows)
    }
  }
}
