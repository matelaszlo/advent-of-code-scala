package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day20.{parseParticle, part1, part2}
import com.lmat.adventofcode.year2017.Day20Definitions._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day20Test extends FunSuite with TableDrivenPropertyChecks {
  val rawParticles =
    """p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
      |p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>
    """.stripMargin

  val particles = Seq(
    Particle(0, Coordinates(3, 0, 0), Coordinates(2, 0, 0), Coordinates(-1, 0, 0)),
    Particle(1, Coordinates(4, 0, 0), Coordinates(0, 0, 0), Coordinates(-2, 0, 0))
  )

  test("Parse 1") {
    assert(rawParticles.split("\n").zipWithIndex.toSeq.flatMap { case (row, i) => parseParticle(row, i) } == particles)
  }

  test("Day20 - Part 1") {
    assert(part1(particles) == 0)
  }

  val rawParticles2 =
    """p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
      |p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
      |p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
      |p=<3,0,0>, v=<1,0,0>, a=<0,0,0>
    """.stripMargin

  val particles2 = Seq(
    Particle(0, Coordinates(-6, 0, 0), Coordinates(3, 0, 0), Coordinates(0, 0, 0)),
    Particle(1, Coordinates(-4, 0, 0), Coordinates(2, 0, 0), Coordinates(0, 0, 0)),
    Particle(2, Coordinates(-2, 0, 0), Coordinates(1, 0, 0), Coordinates(0, 0, 0)),
    Particle(3, Coordinates(3, 0, 0), Coordinates(1, 0, 0), Coordinates(0, 0, 0))
  )

  test("Parse 2") {
    assert(rawParticles2.split("\n").zipWithIndex.toSeq.flatMap { case (row, i) => parseParticle(row, i) } == particles2)
  }

  test("Day20 - Part 2") {
    assert(part2(particles2) == 1)
  }
}
