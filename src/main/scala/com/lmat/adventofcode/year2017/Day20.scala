package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimplePuzzle
import com.lmat.adventofcode.year2017.Day20Definitions._
import com.lmat.util.Files.readResource

object Day20Definitions {
  case class Coordinates(x: Int, y: Int, z: Int) {
    def +(other: Coordinates): Coordinates = Coordinates(x + other.x, y + other.y, z + other.z)
  }
  case class Particle(id: Int, position: Coordinates, velocity: Coordinates, acceleration: Coordinates)
}

object Day20 extends SimplePuzzle[Seq[Particle], Int, Int] {
  override def parse(resource: String): Seq[Particle] =
    readResource(resource).zipWithIndex.flatMap { case (row, i) => parseParticle(row, i) }

  def parseParticle(row: String, index: Int): Option[Particle] = {
    val regex = "p=<(.*),(.*),(.*)>, v=<(.*),(.*),(.*)>, a=<(.*),(.*),(.*)>".r
    row match {
      case regex(p1, p2, p3, v1, v2, v3, a1, a2, a3) => Some(Particle(index, Coordinates(p1.toInt, p2.toInt, p3.toInt), Coordinates(v1.toInt, v2.toInt, v3.toInt), Coordinates(a1.toInt, a2.toInt, a3.toInt)))
      case _                                         => None
    }
  }

  /**
    * As we tend toward infinity acceleration is the main force to account for
    * We can greatly reduce our simulation space by only checking particles with the minimum acceleration
    * Running a 1000 frames already give us the correct answer
    */
  override def part1(particles: Seq[Particle]): Int = {
    val zero = Coordinates(0, 0, 0)
    val iterations = 1000

    val minAcc = particles.map(particle => distance(zero, particle.acceleration)).min
    val candidates = particles.filter(particle => distance(zero, particle.acceleration) == minAcc)

    (1 to iterations)
      .foldLeft(candidates)((cs, _) => cs.map(next))
      .minBy(c => distance(zero, c.position)).id
  }

  def next(particle: Particle): Particle = {
    val newVelocity = particle.velocity + particle.acceleration
    val newPosition = particle.position + newVelocity
    Particle(particle.id, newPosition, newVelocity, particle.acceleration)
  }

  def distance(a: Coordinates, b: Coordinates): Int =
    Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z)

  override def part2(particles: Seq[Particle]): Int = {
    val iterations = 1000

    (1 to iterations)
      .foldLeft(particles)((p, _) => removeCollisions(p.map(next)))
      .size
  }

  def removeCollisions(particles: Seq[Particle]): Seq[Particle] =
    particles.groupBy(_.position)
      .filter { case (_, v) => v.size == 1 }
      .flatMap(_._2).toSeq
}
