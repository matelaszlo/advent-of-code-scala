package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day09Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day09Definitions {
  case class Distance(from: String, to: String, distance: Int) {
    def invert: Distance = Distance(to, from, distance)
  }
}

object Day09 extends SimpleCommonPuzzle[Seq[Distance], Int, Int] {

  override def parse(resource: String): Seq[Distance] = readResource(resource).flatMap(parseDistance).toList

  def parseDistance(row: String): Option[Distance] = {
    val distance = "(.*) to (.*) = (.*)".r

    row match {
      case distance(from, to, d) => Some(Distance(from, to, d.toInt))
      case _                     => None
    }
  }

  override def part1(distances: Seq[Distance]): Int =
    shortestRoute(addInverses(distances)).distance

  case class Route(steps: Seq[Distance]) {
    val first: Option[String] = steps.headOption.map(_.from)
    val last:  Option[String] = steps.lastOption.map(_.to)
    val destinations: Set[String] = steps.flatMap(step => Seq(step.from, step.to)).toSet
    val distance: Int = steps.map(_.distance).sum

    def fits(distance: Distance): Boolean =
      (fitsLeft(distance) || fitsRight(distance)) && isNew(distance)

    private def fitsLeft(distance: Distance): Boolean =
      first.isEmpty || first.contains(distance.to)

    private def fitsRight(distance: Distance): Boolean =
      first.isEmpty || last.contains(distance.from)

    private def isNew(distance: Distance): Boolean =
      !(destinations.contains(distance.from) && destinations.contains(distance.to))

    def add(distance: Distance): Route =
      if (fitsLeft(distance)) Route(distance +: steps)
      else Route(steps :+ distance)
  }

  def addInverses(distances: Seq[Distance]): Seq[Distance] =
    distances.flatMap(distance => Seq(distance, distance.invert))

  def shortestRoute(distances: Seq[Distance]): Route = extremeRoute(distances, Ordering.Int)

  /**
    * Algorithm:
    * Build up a full route starting from all possible distances
    * by always adding the min (max) distance route that fits
    * until all the locations are visited
    * Then select the min (max) of the built up routes
    */
  def extremeRoute(distances: Seq[Distance], ordering: Ordering[Int]): Route = {
    implicit val routeOrdering = Ordering.by[Route, Int](_.distance)(ordering)
    implicit val distanceOrdering = Ordering.by[Distance, Int](_.distance)(ordering)

    val allDestinations: Set[String] = distances.flatMap(distance => Seq(distance.from, distance.to)).toSet

    def remove(distances: Seq[Distance], distance: Distance): Seq[Distance] =
      distances.filterNot(r => r == distance || r == distance.invert)

    @tailrec
    def iterate(route: Route, remaining: Seq[Distance]): Route = {

      if (remaining.isEmpty || route.destinations == allDestinations) route
      else {
        val min = remaining.filter(route.fits).min
        iterate(route.add(min), remove(remaining, min))
      }
    }

    distances.map(distance => iterate(Route(Seq(distance)), remove(distances, distance))).min
  }

  override def part2(distances: Seq[Distance]): Int =
    longestRoute(addInverses(distances)).distance

  def longestRoute(distances: Seq[Distance]): Route = extremeRoute(distances, Ordering.Int.reverse)
}
