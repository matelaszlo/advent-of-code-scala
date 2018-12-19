package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2018.Day13Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day13Definitions {
  sealed trait Tile
  case object Empty      extends Tile
  case object Straight   extends Tile
  case object CurveRight extends Tile
  case object CurveLeft  extends Tile
  case object Crossing   extends Tile

  type Coordinates = (Int, Int)
  type Tracks = Map[Coordinates, Tile]

  sealed trait Heading
  case object Up    extends Heading
  case object Right extends Heading
  case object Down  extends Heading
  case object Left  extends Heading

  sealed trait Decision
  case object HeadStraight extends Decision
  case object TurnLeft extends Decision
  case object TurnRight extends Decision

  case class Cart(coordinates: Coordinates, heading: Heading, decision: Decision){
    val (x, y) = coordinates
  }
}

object Day13 extends SimpleCommonPuzzle[(Tracks, Seq[Cart]), String, String] {
  override def parse(resource: String): (Tracks, Seq[Cart]) = parseInput(readResource(resource))

  def parseInput(rows: Seq[String]): (Tracks, Seq[Cart]) = {
    val (tracks, carts) = rows.zipWithIndex.map{case (row, i) => parseRow(row, i)}.unzip
    (tracks.flatten.toMap, carts.flatten)
  }

  def parseRow(row: String, rowNum: Int): (Seq[(Coordinates, Tile)], Seq[Cart]) = {
    val (tracks, carts) = row.zipWithIndex.map{ case (char, columnNum) => parseChar(char, (columnNum, rowNum))}.unzip
    (tracks, carts.flatten)
  }

  def parseChar(char: Char, coordinates: Coordinates): ((Coordinates, Tile), Option[Cart]) =
    char match {
      case '-'  => ((coordinates, Straight),   None)
      case '|'  => ((coordinates, Straight),   None)
      case '/'  => ((coordinates, CurveRight), None)
      case '\\' => ((coordinates, CurveLeft),  None)
      case '+'  => ((coordinates, Crossing),   None)
      case '>'  => ((coordinates, Straight),   Some(Cart(coordinates, Right, TurnLeft)))
      case '<'  => ((coordinates, Straight),   Some(Cart(coordinates, Left,  TurnLeft)))
      case '^'  => ((coordinates, Straight),   Some(Cart(coordinates, Up,    TurnLeft)))
      case 'v'  => ((coordinates, Straight),   Some(Cart(coordinates, Down,  TurnLeft)))
      case _    => ((coordinates, Empty),      None)
    }

  override def part1(input: (Tracks, Seq[Cart])): String = {
    val (tracks, carts) = input
    val (_, collisions) = Stream.iterate((carts, Seq[Coordinates]())) { case (car, col) => simulateTick(tracks)(car, col) }.find(_._2.nonEmpty).get
    val (x, y) = collisions.head
    s"$x,$y"
  }

  def simulateTick(tracks: Tracks)(carts: Seq[Cart], collisions: Seq[Coordinates]): (Seq[Cart], Seq[Coordinates]) = {
    @tailrec
    def simulate(toSimulate: Seq[Cart], collisions: Seq[Coordinates], remaining: Seq[Cart]): (Seq[Cart], Seq[Coordinates]) = toSimulate match {
      case cart +: rest =>
        val moved = move(tracks)(cart)
        val isCollided = checkCollisions(moved, remaining ++ toSimulate)
        if (isCollided) simulate(rest.filterNot(_.coordinates == moved.coordinates), collisions :+ moved.coordinates, remaining.filterNot(_.coordinates == moved.coordinates))
        else simulate(rest, collisions, remaining :+ moved)
      case _ => (remaining, collisions)
    }

    val orderedCarts = carts.sortBy(c => (c.coordinates._2, c.coordinates._1))
    simulate(orderedCarts, collisions, Seq())
  }

  def move(tracks: Tracks)(cart: Cart): Cart = (tracks(cart.coordinates), cart.heading) match {
    case (Straight, _)       => moveForward(cart)

    case (CurveRight, Up)    => moveForward(turnRight(cart))
    case (CurveRight, Right) => moveForward(turnLeft(cart))
    case (CurveRight, Down)  => moveForward(turnRight(cart))
    case (CurveRight, Left)  => moveForward(turnLeft(cart))

    case (CurveLeft, Up)     => moveForward(turnLeft(cart))
    case (CurveLeft, Right)  => moveForward(turnRight(cart))
    case (CurveLeft, Down)   => moveForward(turnLeft(cart))
    case (CurveLeft, Left)   => moveForward(turnRight(cart))

    case (Crossing, _)       => moveForward(decideHeading(cart))
    case (Empty, _ )         => throw new IllegalArgumentException(s"Cart got derailed! $cart")
  }

  def moveForward(cart: Cart): Cart = cart.heading match {
    case Up    => cart.copy(coordinates = (cart.x,     cart.y - 1))
    case Right => cart.copy(coordinates = (cart.x + 1, cart.y))
    case Down  => cart.copy(coordinates = (cart.x,     cart.y + 1))
    case Left  => cart.copy(coordinates = (cart.x - 1, cart.y))
  }

  def turnRight(cart: Cart): Cart = cart.heading match {
    case Up    => cart.copy(heading = Right)
    case Right => cart.copy(heading = Down)
    case Down  => cart.copy(heading = Left)
    case Left  => cart.copy(heading = Up)
  }

  def turnLeft(cart: Cart): Cart = cart.heading match {
    case Up    => cart.copy(heading = Left)
    case Left  => cart.copy(heading = Down)
    case Down  => cart.copy(heading = Right)
    case Right => cart.copy(heading = Up)
  }

  def decideHeading(cart: Cart): Cart = cart.decision match {
    case TurnLeft     => turnLeft(cart.copy(decision = HeadStraight))
    case HeadStraight => cart.copy(decision = TurnRight)
    case TurnRight    => turnRight(cart.copy(decision = TurnLeft))
  }

  def checkCollisions(cart: Cart, rest: Seq[Cart]): Boolean =
    rest.exists(_.coordinates == cart.coordinates)

  override def part2(input: (Tracks, Seq[Cart])): String = {
    val (tracks, carts) = input
    val (cart +: _, _) = Stream.iterate((carts, Seq[Coordinates]())) { case (car, col) => simulateTick(tracks)(car, col) }.find(_._1.size == 1).get
    val (x, y) = cart.coordinates
    s"$x,$y"
  }
}
