package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day10Definitions._
import com.lmat.util.Files.readResource

object Day10Definitions {
  type Maze = Map[Coordinate, Tile]

  case class Coordinate(x: Int, y: Int)

  sealed trait Tile
  case object Ground extends Tile
  case class Pipe(from: Direction, to: Direction) extends Tile
  case object Start extends Tile

  sealed trait Direction
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction
}

object Day10 extends SimpleCommonPuzzle[Maze, Int, Int] {
  override def parse(resource: String): Maze =
    parseMaze(readResource(resource).toList)

  def parseMaze(rows: List[String]): Maze =
    rows.zipWithIndex
      .flatMap { case (row, y) => row.toCharArray.toList.zipWithIndex
        .map { case (c, x) => (Coordinate(x, y), parseTile(c)) }
      }
      .toMap

  def parseTile(c: Char): Tile = c match {
    case '|' => Pipe(North, South)
    case '-' => Pipe(West, East)
    case 'L' => Pipe(North, East)
    case 'J' => Pipe(North, West)
    case '7' => Pipe(South, West)
    case 'F' => Pipe(South, East)
    case 'S' => Start
    case _   => Ground
  }

  // Generate the routes in both directions
  // Take the minimum index for each coordinate
  // Get the max of the distances
  override def part1(maze: Maze): Int = {
    val (startCoordinates, startPipe) = findStartPipe(maze)
    val updatedMaze = maze.updated(startCoordinates, startPipe)
    val route1 = route(updatedMaze)(startCoordinates, startPipe.from)
    val route2 = route(updatedMaze)(startCoordinates, startPipe.to)

    val distanceMap = (route1.zipWithIndex ++ route2.zipWithIndex).groupBy(_._1).map { case (k, v) => (k, v.map(_._2).min) }
    distanceMap.maxBy(_._2)._2
  }

  // Given a maze where the start is replaced by a correctly facing pipe
  // The start coordinates and a direction to go in
  // We iterate until we get back to where we started from
  def route(maze: Maze)(start: Coordinate, direction: Direction): List[Coordinate] = {
      def next(current: (Coordinate, Direction)): (Coordinate, Direction) = {
        val (coordinate, direction) = current
        val nextCoordinate = direction match {
          case North => coordinate.copy(y = coordinate.y - 1)
          case East  => coordinate.copy(x = coordinate.x + 1)
          case South => coordinate.copy(y = coordinate.y + 1)
          case West  => coordinate.copy(x = coordinate.x - 1)
        }
        val nextTile = maze.getOrElse(nextCoordinate, Ground).asInstanceOf[Pipe]
        val nextDirection = if(nextTile.from == inverse(direction)) nextTile.to else nextTile.from
        (nextCoordinate, nextDirection)
      }

    val rest = LazyList.iterate((start, direction))(next).drop(1).takeWhile { case (c, _) => c != start }.map(_._1).toList
    start :: rest
  }

  // We find the start pipe and based on its neighbours figure out which direction it is
  def findStartPipe(maze: Maze): (Coordinate, Pipe) = {
    val directions = List(North, South, West, East)

    val coordinates = maze.find(_._2 == Start).map(_._1).getOrElse(Coordinate(0, 0))
    val neighbours: List[(Int, Int, Direction)] = List((0, -1, North), (-1, 0, West), (1, 0, East), (0, 1, South))
    val connections =
      neighbours.map { case (x, y, direction) => (maze.getOrElse(Coordinate(coordinates.x + x, coordinates.y + y), Ground), direction) }
        .collect {
          case (Pipe(from, _), direction) if inverse(direction) == from => direction
          case (Pipe(_, to), direction) if inverse(direction) == to => direction
        }.sortBy(d => directions.indexOf(d))

    (coordinates, Pipe(connections.head, connections(1)))
  }

  def inverse(direction: Direction): Direction = direction match {
    case North => South
    case East => West
    case South => North
    case West => East
  }

  // Generate a route and find all enclosed tiles
  override def part2(maze: Maze): Int = {
    val (startCoordinates, startPipe) = findStartPipe(maze)
    val updatedMaze = maze.updated(startCoordinates, startPipe)
    val theRoute = route(updatedMaze)(startCoordinates, startPipe.from)
    val inside = enclosed(updatedMaze)(theRoute)
    inside.size
  }

  // A non-route tile is enclosed if from a set direction the number of crossed pipes sections are odd
  // E.g going West to East ->
  // Pipe(North, South) | is 1
  // Pipe(South, East) => Pipe(North, West) F-----J and Pipe(North, East) => Pipe(South, West) L----7 are both 1 // Visualize them as 1 pipe
  // Pipe(South, East) => Pipe(South, West) F-----7 and Pipe(North, East) => Pipe(North, West) L----J are both 2 // Visualize them as 2 pipes
  def enclosed(maze: Maze)(route: List[Coordinate]): List[Coordinate] = {
    def boundaryCount(c: Coordinate): Int = {
      val sameRowBefore = route.filter(rC => rC.y == c.y && rC.x < c.x).sortBy(c => (c.y, c.x))
      val boundary = sameRowBefore.foldLeft((0, Option.empty[Pipe])) { case ((count, opened), rC) => maze(rC) match {
        case Pipe(North, South) => (count + 1, opened)
        case Pipe(South, East) => (count, Some(Pipe(South, East)))
        case Pipe(North, East) => (count, Some(Pipe(North, East)))
        case Pipe(North, West) if opened.contains(Pipe(South, East)) => (count + 1, None)
        case Pipe(North, West) if opened.contains(Pipe(North, East)) => (count + 2, None)
        case Pipe(South, West) if opened.contains(Pipe(North, East)) => (count + 1, None)
        case Pipe(South, West) if opened.contains(Pipe(South, East)) => (count + 2, None)
        case _ => (count, opened)
      }}
      boundary._1
    }

    val nonRoute = maze.keysIterator.toList.filterNot(route.contains).sortBy(c => (c.y, c.x))
    nonRoute.filter(c => (boundaryCount(c) % 2) == 1)
  }
}
