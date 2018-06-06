package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimplePuzzle
import com.lmat.util.Files.readResource

object Day03 extends SimplePuzzle[Int, Int, Int] {
  override def parse(resource: String): Int =
    readResource(resource).head.toInt

  /**
    * Algorithm:
    * 0. Given an infinite stream of rings
    * 1. Find which ring the number falls on
    * 2. Calculate the distance from the closest middle point on the ring
    * 3. Add this to the rings distance from center
    *
    * With this approach we do not need to generate the spiral for this part
    */
  override def part1(input: Int): Int =
    if (input == 1) 0
    else {
      val ring = Stream.from(3, 2).map(createRing).find(ring => ring.min <= input && input <= ring.max).get
      val middles = calculateMiddles(ring)
      ring.num + middles.map(middle => Math.abs(middle - input)).min
    }

  case class Ring(num: Int, base:Int, min: Int, max: Int)

  /**
    * The rings are defined by a number lets call it base
    * The base is an odd number starting from 3
    *
    * Ring 1 has base 3 min 2 and max 9
    * Ring 2 has base 5 min 10 and max 25
    */
  def createRing(base: Int): Ring =
    Ring((base - 1) / 2, base, Math.pow(base - 2, 2).toInt + 1, Math.pow(base, 2).toInt)

  def calculateMiddles(ring: Ring): Seq[Int] =
    Stream.from(ring.min + ring.num - 1, 2 * ring.num).take(4).toList

  /**
    * Algorithm:
    * Here we do have to generate the spiral
    * We can model it as an infinite stream of squares
    * Once generated finding the first one larger than the input is trivial
    */
  override def part2(input: Int): Int =
    squares
      .find(square => square.value > input)
      .map(_.value)
      .get

  case class Position(x: Int, y: Int)
  case class Square(position: Position, value: Int)

  def initial: Square = Square(Position(0, 0), 1)
  def squares: Stream[Square] = Stream.iterate(Seq(initial))(next).map(_.head)

  def next(squares: Seq[Square]): Seq[Square] = {
    val pos = nextPosition(squares.map(_.position))
    val value = neighbours(squares, pos).map(_.value).sum
    Square(pos, value) +: squares
  }

  def nextPosition(positions: Seq[Position]): Position = {
    val current +: past = positions

    val isLeft  = past.contains(left(current))
    val isRight = past.contains(right(current))
    val isUp    = past.contains(up(current))
    val isDown  = past.contains(down(current))

    (isLeft, isRight, isUp, isDown) match {
      case (false, false, false, false) => right(current)

      case (true,  false, false, false) => up(current)
      case (true,  false, false, true)  => up(current)

      case (false, false, false, true)  => left(current)
      case (false, true,  false, true)  => left(current)

      case (false, true,  false, false) => down(current)
      case (false, true,  true,  false) => down(current)

      case (false, false, true,  false) => right(current)
      case (true,  false, true,  false) => right(current)

      case _ => throw new IllegalArgumentException(s"Unexpected position isLeft=$isLeft, isRight=$isRight, isUp=$isUp, isDown=$isDown")
    }
  }

  def left  (position: Position): Position = Position(position.x - 1, position.y)
  def right (position: Position): Position = Position(position.x + 1, position.y)
  def up    (position: Position): Position = Position(position.x, position.y + 1)
  def down  (position: Position): Position = Position(position.x, position.y - 1)

  def neighbours(squares: Seq[Square], position: Position): Seq[Square] = {
    val positions = neighbours(position)
    squares.filter(square => positions.contains(square.position))
  }

  def neighbours(position: Position): Seq[Position] =
    for {
      x <- -1 to 1
      y <- -1 to 1 if !(x == 0 && y == 0)
    } yield Position(position.x + x, position.y + y)
}
