package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Matrix
import com.lmat.adventofcode.year2017.Day21Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix.{flipHorizontal, flipVertical, rotateLeft, rotateRight}

object Day21Definitions {
  type Pattern = Matrix[Boolean]
  case class Rule(from: Pattern, to: Pattern)
}

object Day21 extends SimpleCommonPuzzle[Set[Rule], Int, Int]{
  override def parse(resource: String): Set[Rule] =
    readResource(resource).map(parseRule).toSet.flatMap(expandRule)

  def buildPattern(matrix: Seq[Seq[Char]]): Pattern =
    Matrix(matrix.map(_.map(_ == '#').toVector).toVector)

  def buildPattern(pattern: String): Pattern =
    buildPattern(pattern.split("/").map(_.toIndexedSeq).toIndexedSeq)

  def parseRule(row: String): Rule = {
    val parts = row.split(" => ")
    Rule(buildPattern(parts(0)), buildPattern(parts(1)))
  }

  /**
    * We expand the rule set with all possible transformations up front
    */
  def expandRule(rule: Rule): Set[Rule] = equivalentPatterns(rule.from).map(from => Rule(from, rule.to))

  /**
    * There are 8 unique rotation/flip combinations
    */
  def equivalentPatterns(pattern: Pattern): Set[Pattern] = Set(
    pattern,
    flipHorizontal(pattern),
    flipVertical(pattern),
    rotateLeft(pattern),
    rotateRight(pattern),
    rotateRight(rotateRight(pattern)),
    rotateRight(flipVertical(pattern)),
    rotateLeft(flipVertical(pattern))
  )

  val initialPattern: Pattern =
    buildPattern(".#./..#/###")

  /**
    * There are lots of useful Matrix libraries but it is educational to try and write the transformations without them
    * I have optimized for understandability on these rather than performance as we don't strictly need it for the puzzle
    */
  override def part1(rules: Set[Rule]): Int = pixelsOn(applyRules(rules, initialPattern, 5))

  def applyRules(rules: Set[Rule], start: Pattern, iterations: Int): Pattern =
    (0 until iterations).foldLeft(start)((pattern, _) => {
      val divisor = if (pattern.rows.size % 2 == 0) 2 else 3
      val parts = Matrix.break(pattern, divisor, divisor)
      val updated = parts.map(pattern => rules.find(rule => rule.from == pattern).get.to)
      Matrix.merge(updated)
    })

  def pixelsOn(pattern: Pattern): Int =
    pattern.rows.map(_.count(identity)).sum

  override def part2(rules: Set[Rule]): Int = pixelsOn(applyRules(rules, initialPattern, 18))
}
