package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day14 extends SimpleCommonPuzzle[(String, Map[String, String]), Int, BigInt] {
  override def parse(resource: String): (String, Map[String, String]) =
    parseInstructions(readResource(resource))

  def parseInstructions(rows: Seq[String]): (String, Map[String, String]) = {
    val template = rows.head
    val rules = rows.drop(2).flatMap(parseRule).toMap
    (template, rules)
  }

  def parseRule(row: String): Option[(String, String)] = {
    val rule = s"(.*) -> (.*)".r
    row match {
      case rule(from, to) => Some(from, to)
      case _ => None
    }
  }

  // Naive implementation
  override def part1(input: (String, Map[String, String])): Int = {
    val (template, rules) = input
    val polymer = polymerization(rules)(template).drop(10).head
    val ((_, least), (_, most)) = leastAndMostCommon(polymer)
    most - least
  }

  def polymerization(rules: Map[String, String])(template: String): LazyList[String] =
    LazyList.iterate(template)(step(rules))

  // We first split the polymer with a sliding iterator to pairs
  // Then we expand all pairs according to the rules
  // We drop the first character of all pairs except the first one to avoid duplications
  // We merge the pairs back to a string
  def step(rules: Map[String, String])(polymer: String): String = {
    val expanded = polymer.sliding(2).map(pair => rules.get(pair) match {
      case Some(element) => pair.patch(1, element, 0)
      case None => pair
    }).toList
    (expanded.head :: expanded.tail.map(_.drop(1))).mkString
  }

  def leastAndMostCommon(polymer: String): ((Char, Int), (Char, Int)) = {
    val occurrenceMap = polymer.groupBy(identity).view.mapValues(_.length)
    (occurrenceMap.minBy(_._2), occurrenceMap.maxBy(_._2))
  }

  // The naive implementation from part1 does not scale well enough
  // Here we have to forego knowing the exact polymer and just keep track of the count of the pairs contained in it
  // We also switch to BigInt to avoid overflow
  override def part2(input: (String, Map[String, String])): BigInt = {
    val (template, rules) = input
    val polymerPairs = polymerization2(rules)(template).drop(40).head
    val ((_, least), (_, most)) = leastAndMostCommon2(template.substring(0, 1))(polymerPairs)
    most - least
  }

  def polymerization2(rules: Map[String, String])(template: String): LazyList[Map[String, BigInt]] = {
    val pairs = template.sliding(2).toList.groupBy(identity).view.mapValues(l => BigInt(l.length)).toMap
    LazyList.iterate(pairs)(step2(rules))
  }

  // We keep every pair that is unaffected by rules
  // We split every pair into two that is affected by the rules
  // We recalculate the counts with a fold
  def step2(rules: Map[String, String])(pairs: Map[String, BigInt]): Map[String, BigInt] = {
    pairs.toList.flatMap { case (pair, count) => rules.get(pair) match {
      case Some(element) => List((pair.patch(0, element, 1), count), (pair.patch(1, element, 1), count))
      case None => List((pair, count))
    }}.foldLeft(Map.empty[String, BigInt]) { case (state, (p, c)) => state.updated(p, state.getOrElse(p, BigInt(0)) + c) }
  }

  // We need to calculate individual counts from the pair counts
  // We can avoid duplicates by only counting the second character in every pair and then correcting the first character count
  def leastAndMostCommon2(firstPair: String)(polymerPairs: Map[String, BigInt]): ((Char, BigInt), (Char, BigInt)) = {
    val occurrenceMap = polymerPairs.toList.map { case (pair, count) => (pair.last, count) }
      .foldLeft(Map.empty[Char, BigInt]) { case (state, (p, c)) => state.updated(p, state.getOrElse(p, BigInt(0)) + c) }
    val corrected = occurrenceMap.updated(firstPair.head, occurrenceMap.getOrElse(firstPair.head, BigInt(0)) + polymerPairs.getOrElse(firstPair, BigInt(0)))
    (corrected.minBy(_._2), corrected.maxBy(_._2))
  }
}
