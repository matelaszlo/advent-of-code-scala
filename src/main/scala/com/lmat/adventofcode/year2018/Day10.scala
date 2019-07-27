package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2018.Day10Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day10Definitions {
  case class Coordinate(x: Int, y: Int){
    def +(other: Coordinate): Coordinate = Coordinate(x + other.x, y + other.y)
  }
  case class Point(position: Coordinate, velocity: Coordinate)
}

object Day10 extends SimpleCommonPuzzle[Seq[Point], String, Int] {
  override def parse(resource: String): Seq[Point] = readResource(resource).flatMap(parsePoint).toVector

  def parsePoint(row: String): Option[Point] = {
    val pattern = "position=<(.*?),(.*?)> velocity=<(.*?),(.*?)>".r
    row match {
      case pattern(pXS, pYS, vXS, vYS) => (for {
        pX <- Try(pXS.trim.toInt)
        pY <- Try(pYS.trim.toInt)
        vX <- Try(vXS.trim.toInt)
        vY <- Try(vYS.trim.toInt)
      } yield Point(Coordinate(pX, pY), Coordinate(vX, vY))).toOption
      case _ => None
    }
  }

  def printCoordinates(coordinates: Seq[Coordinate]): String = {
    val (min, max) = defineSquare(coordinates)
    (min.y to max.y).map(printRow(_, min.x, max.x)(coordinates)).mkString("\n")
  }

  def printRow(row: Int, min: Int, max: Int)(coordinates: Seq[Coordinate]): String =
    (min to max).map(column => if (coordinates.contains(Coordinate(column, row))) '#' else '.').mkString

  def defineSquare(coordinates: Seq[Coordinate]): (Coordinate, Coordinate) = {
    val xs = coordinates.map(_.x)
    val ys = coordinates.map(_.y)
    (Coordinate(xs.min, ys.min), Coordinate(xs.max, ys.max))
  }

  override def part1(points: Seq[Point]): String = {
    val n = 1000
    println(s"Ignoring the initial frames that are larger than $n")
    val (candidate, _) = skipLargeFrames(n)(points)

    // Without NLP this part has to be manually solved
    // We can help by printing candidate frames that are smaller sized than a limit (correct limit can be found by experimentation)
    pointsStream(candidate).takeWhile(size(_) <= n).map(_.map(_.position)).map(printCoordinates).foreach(println)
    "JJXZHKFP"
  }

  def skipLargeFrames(n: Int)(start: Seq[Point]): (Seq[Point], Int)  = {
    @tailrec
    def iterate(n: Int)(points: Seq[Point], count: Int): (Seq[Point], Int) =
      if(size(points) > n) iterate(n)(next(points), count + 1)
      else (points, count)

    iterate(n)(start, 0)
  }

  def pointsStream(points: Seq[Point]): LazyList[Seq[Point]] =
    LazyList.iterate(points)(next)

  def next(points: Seq[Point]): Seq[Point] =
    points.map(next)

  def next(point: Point): Point =
    Point(point.position + point.velocity, point.velocity)

  def size(points: Seq[Point]): Long = {
    val (min, max) = defineSquare(points.map(_.position))
    Math.abs(max.x.toLong - min.x.toLong) * Math.abs(max.y.toLong - min.y.toLong)
  }

  override def part2(points: Seq[Point]): Int =
    skipLargeFrames(1000)(points)._2
}
