package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.adventofcode.year2018.Day03Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day03Definitions {
  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int)
  case class Pixel(left: Int, top: Int)

  case class SuitPlan(claims: Seq[Claim], coverMap: Map[Pixel, Int])
}

object Day03 extends CommonPuzzle[Seq[Claim], SuitPlan, Int, Int] {
  override def parse(resource: String): Seq[Claim] = readResource(resource).flatMap(parseClaim)

  def parseClaim(row: String): Option[Claim] = {
    val pattern = "#(.*?) @ (.*?),(.*?): (.*?)x(.*)".r

    row match {
      case pattern(idS, leftS, topS, widthS, heightS) => (for {
        id     <- Try(idS.toInt)
        left   <- Try(leftS.toInt)
        top    <- Try(topS.toInt)
        width  <- Try(widthS.toInt)
        height <- Try(heightS.toInt)
      } yield Claim(id, left, top, width, height)).toOption
      case _ => None
    }
  }

  override def preProcess(claims: Seq[Claim]): SuitPlan =
    SuitPlan(claims, buildCoverMap(claims))

  override def part1(plan: SuitPlan): Int =
    plan.coverMap.values.count(_ > 1)

  def buildCoverMap(claims: Seq[Claim]): Map[Pixel, Int] = {
    @tailrec
    def iterate(remaining: Seq[Claim], covered: Map[Pixel, Int]): Map[Pixel, Int] = remaining match {
      case h +: t =>
        val pixelsH = pixels(h)
        iterate(t, incrementAll(covered, pixelsH))
      case _ => covered
    }
    iterate(claims, Map())
  }

  def incrementAll[K](map: Map[K, Int], keys: Set[K]): Map[K, Int] =
    keys.foldLeft(map)(increment)

  def increment[K](map: Map[K, Int], key: K): Map[K, Int] =
    map.updated(key, map.getOrElse(key, 0) + 1)

  def pixels(claim: Claim): Set[Pixel] = (for {
    l <- claim.left until (claim.left + claim.width)
    t <- claim.top until (claim.top + claim.height)
  } yield Pixel(l, t)).toSet

  override def part2(plan: SuitPlan): Int =
    plan.claims.find(claim => pixels(claim).forall(plan.coverMap(_) == 1)).map(_.id).get
}
