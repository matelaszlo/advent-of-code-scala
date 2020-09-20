package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day17Definitions._
import com.lmat.adventofcode.CommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day17Definitions {

  sealed trait ClayCoordinates
  case class ClayCoordinatesHorizontal(x: Int, yMin: Int, yMax: Int) extends ClayCoordinates
  case class ClayCoordinatesVertical(y: Int, xMin: Int, xMax: Int) extends ClayCoordinates
  case class Coordinate(x: Int, y: Int)

  sealed trait Tile
  case object Sand extends Tile
  case object Clay extends Tile
  case object Water extends Tile
  case object WaterSettled extends Tile

  case class ReservoirState(tiles: Map[Coordinate, Tile]){
    def get(coordinate: Coordinate): Tile = tiles.getOrElse(coordinate, Sand)
    def get(tile: Tile): Set[(Coordinate, Tile)] =
      tiles.filter {
        case (_, t) => t == tile
        case _      => false
      }.toSet

    def updated(coordinate: Coordinate, tile: Tile): ReservoirState =
      ReservoirState(tiles.updated(coordinate, tile))
    def updated(contents: Set[(Coordinate, Tile)]): ReservoirState =
      ReservoirState(contents.foldLeft(tiles){ case (ts, (c, t)) => ts.updated(c, t) })

    def bound(yMax: Int): ReservoirState =
      ReservoirState(tiles.filter { case (Coordinate(_, y), _) => y <= yMax })
    def bound(yMin: Int, yMax: Int): ReservoirState =
      ReservoirState(tiles.filter { case (Coordinate(_, y), _) => y >= yMin && y <= yMax })
  }
}

object Day17 extends CommonPuzzle[Set[ClayCoordinates], ReservoirState, Int, Int] {
  override def parse(resource: String): Set[ClayCoordinates] = readResource(resource).flatMap(parseRow).toSet

  def parseRow(row: String): Option[ClayCoordinates] = {
    val pattern = s"x=(.*), y=(.*)\\.\\.(.*)".r
    val pattern2 = s"y=(.*), x=(.*)\\.\\.(.*)".r
    row match {
      case pattern(xRaw, yMinRaw, yMaxRaw) => for {
        x <- Try(xRaw.toInt).toOption
        yMin <- Try(yMinRaw.toInt).toOption
        yMax <- Try(yMaxRaw.toInt).toOption
      } yield ClayCoordinatesHorizontal(x, yMin, yMax)
      case pattern2(yRaw, xMinRaw, xMaxRaw) => for {
        y <- Try(yRaw.toInt).toOption
        xMin <- Try(xMinRaw.toInt).toOption
        xMax <- Try(xMaxRaw.toInt).toOption
      } yield ClayCoordinatesVertical(y, xMin, xMax)
      case _ => None
    }
  }

  // Since both parts require the fully run and bounded simulation we do it in preProcess
  override def preProcess(coordinates: Set[ClayCoordinates]): ReservoirState = {
    val reservoirState = build(coordinates)
    val yMin = reservoirState.tiles.keys.map(_.y).min
    val yMax = reservoirState.tiles.keys.map(_.y).max
    simulate(reservoirState.updated(Coordinate(500, 0), Water)).bound(yMin, yMax)
  }

  // Build a ReservoirState from the listed ClayCoordinates
  def build(coordinates: Set[ClayCoordinates]): ReservoirState = {
    val tiles: Set[(Coordinate, Tile)] =  coordinates
      .flatMap {
        case ClayCoordinatesHorizontal(x, yMin, yMax) => (yMin to yMax).map(y => Coordinate(x, y))
        case ClayCoordinatesVertical(y, xMin, xMax) => (xMin to xMax).map(x => Coordinate(x, y))
      }.map((_, Clay))
    ReservoirState(Map[Coordinate, Tile]()).updated(tiles)
  }

  // We simulate an infinite stream of reservoir states and stop when the state has not changed (when bounded to max Y present in the dataset)
  def simulate(reservoirState: ReservoirState): ReservoirState = {
    val yMax = reservoirState.tiles.keys.map(_.y).max
    val reservoir = LazyList.iterate(reservoirState)(flow(yMax))
    val result = (reservoir zip reservoir.drop(1)).find { case (current, next) => current.bound(yMax) == next.bound(yMax) }.map(_._1).get
    println(s"\n Reservoir State:\n${print(result)}")
    result
  }

  // To simplify the problem we break it down to 2 cases
  // Free fall until we hit clay
  // Fill a reservoir
  // We update one stream of water at every step sorted by (x, y)
  def flow(yMax: Int)(state: ReservoirState): ReservoirState = {
    def isBlocking(tile: Tile) = tile match {
      case Sand         => false
      case Water        => false
      case Clay         => true
      case WaterSettled => true
    }

    // We simulate free falling water  up to the point of almost hitting something blocking
    def freeFall(x: Int, y: Int): Set[(Coordinate, Tile)] =
      LazyList.iterate(Coordinate(x, y + 1)) { case Coordinate(x, y) => Coordinate(x, y + 1) }
        .map(c => (c, state.get(c)))
        .takeWhile { case (Coordinate(_, y), t) => !isBlocking(t) && y <= yMax + 1 }
        .dropRight(1)
        .map { case (c, _) => (c, Water) }.toSet

    // We simulate settling water until it overflows in one or both directions
    def fillReservoir(x: Int, y: Int): Set[(Coordinate, Tile)] = {
      // From the starting x coordinate go both ways until hitting a wall or overflowing (keep one overflow tile)
      def settleRow(sC: ReservoirState, yC: Int): Set[(Coordinate, Tile)] = {
        val left = LazyList.iterate(Coordinate(x, yC))(c => Coordinate(c.x - 1, c.y)).takeWhile(c => sC.get(c) != Clay && isBlocking(sC.get(Coordinate(c.x, yC + 1)))).toSet
        val right = LazyList.iterate(Coordinate(x, yC))(c => Coordinate(c.x + 1, c.y)).takeWhile(c => sC.get(c) != Clay && isBlocking(sC.get(Coordinate(c.x, yC + 1)))).toSet

        val xLC = left.map(_.x).minOption.getOrElse(x) - 1
        val xRC = right.map(_.x).maxOption.getOrElse(x) + 1
        val leftOverflow = if(!isBlocking(sC.get(Coordinate(xLC, yC)))) left + Coordinate(xLC, yC) else left
        val rightOverflow = if(!isBlocking(sC.get(Coordinate(xRC, yC)))) right + Coordinate(xRC, yC) else right

        (leftOverflow ++ rightOverflow).map(c => (c, WaterSettled))
      }

      def notOverflowing(sC: ReservoirState, rC: Set[(Coordinate, Tile)]): Boolean = {
        val yC = rC.map(_._1.y).min
        val xLC = rC.map(_._1.x).min
        val xRC = rC.map(_._1.x).max
        isBlocking(sC.get(Coordinate(xLC, yC + 1))) && isBlocking(sC.get(Coordinate(xRC, yC + 1)))
      }

      // Infinitely settle rows of water until hitting a row that overflows
      val settledRows = LazyList.iterate((y + 2, state, Set[(Coordinate, Tile)]())){ case (i, s, _) =>
        val newI = i - 1
        val newR = settleRow(s, newI)
        val newS = s.updated(newR)
        (newI, newS, newR)
      }.drop(1).takeWhile{ case (_, sC, rC) => notOverflowing(sC, rC)}

      // Add the overflow row
      val (lY, lS, _) = settledRows.lastOption.getOrElse((y + 2, state, Set()))
      val overflow: Set[(Coordinate, Tile)] = settleRow(lS, lY -1).map{case (c, _) => (c, Water)}
      (settledRows.map(_._3) :+ overflow).toSet.flatten
    }

    val waterEnds = state.get(Water).filter { case (Coordinate(x, y), _) => state.get(Coordinate(x, y + 1)) == Sand && y < yMax }
    val currentWaterEnd = (if (waterEnds.isEmpty) None else Some(waterEnds.minBy { case (Coordinate(x, y), _) => (x, y) })).toSet
    val newWaterTiles = currentWaterEnd.flatMap { case (Coordinate(x, y), _) => if (isBlocking(state.get(Coordinate(x, y + 2)))) fillReservoir(x, y) else freeFall(x, y) }
    state.updated(newWaterTiles)

    // Add this print statement if you want to see progression
    // println(s"\n Reservoir State:\n${print(newState)}")
  }

  override def part1(reservoirState: ReservoirState): Int =
    count(reservoirState, Set(Water, WaterSettled))

  override def part2(reservoirState: ReservoirState): Int =
    count(reservoirState, Set(WaterSettled))

  def count(state: ReservoirState, tiles: Set[Tile]): Int =
    state.tiles.values.count(tiles.contains)

  def print(tile: Tile): String = tile match {
    case Sand         => "."
    case Clay         => "#"
    case Water        => "|"
    case WaterSettled => "~"
  }

  def print(state: ReservoirState): String = {
    val xMin = state.tiles.keys.map(_.x).min
    val xMax = state.tiles.keys.map(_.x).max
    val yMin = state.tiles.keys.map(_.y).min
    val yMax = state.tiles.keys.map(_.y).max
    (yMin to yMax).map(y => (xMin to xMax).map(x => print(state.get(Coordinate(x, y)))).mkString("")).mkString("\n")
  }
}
