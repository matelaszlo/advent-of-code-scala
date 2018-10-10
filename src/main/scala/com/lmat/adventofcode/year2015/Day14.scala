package com.lmat.adventofcode.year2015
import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day14Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Sequences._

object Day14Definitions {
  case class Reindeer(name: String, speed: Int, flyDuration: Int, restDuration:Int)
}

object Day14 extends SimpleCommonPuzzle[List[Reindeer], Int, Int]{
  override def parse(resource: String): List[Reindeer] = readResource(resource).flatMap(parseReindeer).toList

  def parseReindeer(row: String): Option[Reindeer] = {
    val reindeer = "(.*) can fly (.*) km/s for (.*) seconds, but then must rest for (.*) seconds.".r

    row match {
      case reindeer(name, speed, flyDuration, restDuration) => Some(Reindeer(name, speed.toInt, flyDuration.toInt, restDuration.toInt))
      case _                                                => None
    }
  }

  /**
    * The reindeer movements can easily be modelled as an infinite stream
    * After taking the cumulative stream of the movements the solution is rather elegant
    */
  override def part1(reindeers: List[Reindeer]): Int =
    reindeers.map(cumulativeTravelledStream(_)(2503)).max

  def travelledStream(reindeer: Reindeer): Stream[Int] =
    cycle(Seq.fill(reindeer.flyDuration)(reindeer.speed) ++ Seq.fill(reindeer.restDuration)(0))

  def cumulativeTravelledStream(reindeer: Reindeer): Stream[Int] =
    cumulative[Int](0, _ + _)(travelledStream(reindeer))

  /**
    * We can solve part 2 with the same approach as long as we can merge the movement streams together into a stream of points
    */
  override def part2(reindeers: List[Reindeer]): Int =
    cumulativePointsStream(reindeers)(2503).max

  def pointsStream(reindeers: List[Reindeer]): Stream[List[Int]] =
    merge(reindeers.map(cumulativeTravelledStream(_).drop(1))).map(toPoints)

  def toPoints(distances:List[Int]): List[Int] = {
    val max = distances.max
    distances.map(d => if(d == max) 1 else 0)
  }

  def cumulativePointsStream(reindeers: List[Reindeer]): Stream[List[Int]] =
    cumulative[List[Int]](List.fill(reindeers.size)(0), merge)(pointsStream(reindeers))

  def merge(a: List[Int], b: List[Int]): List[Int] =
    (a zip b).map { case (x, y) => x + y }

  def merge[A](streams: List[Stream[A]]): Stream[List[A]] =
    Stream.from(0).map(i => streams.map(_(i)))
}
