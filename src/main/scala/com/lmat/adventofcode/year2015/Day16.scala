package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day16Definitions._
import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day16Definitions {
  case class Aunt(name: String, number: Int, attributes: Map[String, Int])
}

object Day16 extends SimpleCommonPuzzle[Seq[Aunt], Int, Int] {
  override def parse(resource: String): Seq[Aunt] = readResource(resource).flatMap(parseAunt)

  def parseAunt(row: String): Option[Aunt] = {
    val aunt = "(.*?) (.*?): (.*)".r

    row match {
      case aunt(name, number, attributes) => Some(Aunt(name, number.toInt, parseAttributes(attributes)))
      case _                              => None
    }
  }

  def parseAttributes(raw: String): Map[String, Int] =
    raw.split(',').map(a => {
      val parts = a.split(": ")
      (parts.head.trim, parts(1).toInt)
    }).toMap

  /**
    * By filtering out all the aunts with wrong attributes we can easily find the right one
    */
  override def part1(aunts: Seq[Aunt]): Int =
    aunts.filterNot(aunt => aunt.attributes.exists { case (attr, num) => clues(attr) != num }).head.number

  lazy val clues: Map[String, Int] = Map(
    "children"    -> 3,
    "cats"        -> 7,
    "samoyeds"    -> 2,
    "pomeranians" -> 3,
    "akitas"      -> 0,
    "vizslas"     -> 0,
    "goldfish"    -> 5,
    "trees"       -> 3,
    "cars"        -> 2,
    "perfumes"    -> 1)

  override def part2(aunts: Seq[Aunt]): Int ={
    aunts.filter(aunt => aunt.attributes.forall { case (attr, num) => cluesV2(attr)(num) }).head.number
  }

  lazy val cluesV2: Map[String, Int => Boolean] = Map(
    "children"    -> (_ == 3),
    "cats"        -> (_ >  7),
    "samoyeds"    -> (_ == 2),
    "pomeranians" -> (_ <  3),
    "akitas"      -> (_ == 0),
    "vizslas"     -> (_ == 0),
    "goldfish"    -> (_ <  5),
    "trees"       -> (_ >  3),
    "cars"        -> (_ == 2),
    "perfumes"    -> (_ == 1)
  )
}
