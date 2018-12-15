package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.adventofcode.year2018.Day12Definitions._
import com.lmat.util.Files.readResource

object Day12Definitions {
  case class Rule(from: Seq[Boolean], next: Boolean)
  type RawConfiguration = (Seq[Boolean], Seq[Rule])
  type Configuration = (Set[Int], Set[Seq[Boolean]])
}

object Day12 extends CommonPuzzle[RawConfiguration, Configuration, Int, Long] {
  override def parse(resource: String): RawConfiguration = parseInput(readResource(resource))

  def parseInput(rows: Seq[String]): (Seq[Boolean], Seq[Rule]) = {
    def parseInitial(row: String): Option[Seq[Boolean]] = {
      val pattern = "initial state: (.*?)".r
      row match {
        case pattern(initialS) => Some(initialS.toCharArray.map(_ == '#'))
        case _ => None
      }
    }

    def parseRule(row: String): Option[Rule] = {
      val pattern = "(.*?) => (.)".r
      row match {
        case pattern(fromS, toS) => Some(Rule(fromS.toCharArray.map(_ == '#'), toS.head == '#'))
        case _ => None
      }
    }

    val initialRow +: _ +: ruleRows = rows
    (parseInitial(initialRow).get, ruleRows.flatMap(parseRule))
  }

  override def preProcess(raw: RawConfiguration): Configuration = {
    val (initial, rules) = raw
    (initial.zipWithIndex.filter(_._1).map(_._2).toSet, rules.filter(_.next).map(_.from).toSet)
  }

  override def part1(configuration: Configuration): Int = {
    val (initial, rules) = configuration
    simulate(rules, 20)(initial).sum
  }

  def simulate(rules: Set[Seq[Boolean]], n: Int)(plants: Set[Int]): Set[Int] =
    (1 to n).foldLeft(plants) { case (state, _) => next(rules)(state) }

  def next(rules: Set[Seq[Boolean]])(plants: Set[Int]): Set[Int] = {
    val min = plants.min - 2
    val max = plants.max + 2

    val selectors = -2 to 2

    (min to max).filter(i => {
      val pattern = selectors.map(_ + i).map(plants.contains)
      rules.contains(pattern)
    }).toSet
  }

  /**
    * Since simulating 50000000000 would take took long we need to find a pattern
    * Luckily after around a 100 generations the growth rate stabilizes which we can use to skip simulation of the rest of the cases
    */
  override def part2(configuration: Configuration): Long = {
    val (initial, rules) = configuration

    val value100 = simulate(rules, 100)(initial).sum
    val value200 = simulate(rules, 200)(initial).sum
    val delta = (value200 - value100) / 100

    (50000000000L - 100L) * delta + value100
  }
}
