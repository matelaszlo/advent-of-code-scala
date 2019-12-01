package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.year2018.Day03Definitions._
import com.lmat.adventofcode.year2018.Day03._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day03Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val rawClaims =
    s"""#1 @ 1,3: 4x4
       |#2 @ 3,1: 4x4
       |#3 @ 5,5: 2x2""".stripMargin

  val claims = Seq(
    Claim(1, 1, 3, 4, 4),
    Claim(2, 3, 1, 4, 4),
    Claim(3, 5, 5, 2, 2))

  test("Day03 - Parse") {
    assert(rawClaims.split("\n").toSeq.flatMap(parseClaim) == claims)
  }

  val coverMap = Map(
    Pixel(3,1) -> 1, Pixel(4,1) -> 1, Pixel(5,1) -> 1, Pixel(6,1) -> 1,
    Pixel(3,2) -> 1, Pixel(4,2) -> 1, Pixel(5,2) -> 1, Pixel(6,2) -> 1,
    Pixel(1,3) -> 1, Pixel(2,3) -> 1, Pixel(3,3) -> 2, Pixel(4,3) -> 2, Pixel(5,3) -> 1, Pixel(6,3) -> 1,
    Pixel(1,4) -> 1, Pixel(2,4) -> 1, Pixel(3,4) -> 2, Pixel(4,4) -> 2, Pixel(5,4) -> 1, Pixel(6,4) -> 1,
    Pixel(1,5) -> 1, Pixel(2,5) -> 1, Pixel(3,5) -> 1, Pixel(4,5) -> 1, Pixel(5,5) -> 1, Pixel(6,5) -> 1,
    Pixel(1,6) -> 1, Pixel(2,6) -> 1, Pixel(3,6) -> 1, Pixel(4,6) -> 1, Pixel(5,6) -> 1, Pixel(6,6) -> 1)

  test("Day03 - Build Cover Map") {
    assert(buildCoverMap(claims) == coverMap)
  }

  test("Day03 - Part 1") {
    assert(part1(SuitPlan(claims, coverMap)) == 4)
  }

  test("Day03 - Part 2") {
    assert(part2(SuitPlan(claims, coverMap)) == 3)
  }
}
