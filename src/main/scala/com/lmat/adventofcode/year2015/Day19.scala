package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day19Definitions.{Genetics, Rule}
import com.lmat.util.Files.readResource
import com.lmat.util.Strings.indicesOf

import scala.annotation.tailrec

object Day19Definitions {
  case class Rule(from: String, to: String)
  case class Genetics(molecule: String, rules: Set[Rule])
}

object Day19 extends SimpleCommonPuzzle[Genetics, Int, Int ] {
  override def parse(resource: String): Genetics = parseGenetics(readResource(resource))

  def parseGenetics(rows: Seq[String]): Genetics =
    Genetics(rows.last, rows.dropRight(2).flatMap(parseRule).toSet)

  def parseRule(row: String): Option[Rule] = {
    val rule = "(.*?) => (.*)".r

    row match {
      case rule(from, to) => Some(Rule(from, to))
      case _              => None
    }
  }

  override def part1(genetics: Genetics): Int = nextMolecules(genetics.rules)(genetics.molecule).size

  def nextMolecules(rules: Set[Rule])(molecule: String): Set[String] =
    rules.flatMap(nextMolecules(molecule))

  def nextMolecules(molecule: String)(rule: Rule): Set[String] =
    indicesOf(molecule, rule.from)
      .map{case (from, end) => molecule.substring(0, from) + rule.to + molecule.substring(end , molecule.length)}.toSet

  /**
    * Going from large to small should be more efficient
    * As the problem space is very large we have to eagerly prune the results at every recursive step
    *
    * It turns out that just by applying the largest reduction every step leads to the right solution
    *
    * ToDo: generic algorithm with backtracking
    */
  override def part2(genetics: Genetics): Int = {
    case class Candidate(molecule: String, steps: Int)
    val (Genetics(target, rules), start) = switchRules(genetics, "e")

    def nextCandidates(rules: Set[Rule])(candidate: Candidate): Vector[Candidate] =
      nextMolecules(rules)(candidate.molecule).map(Candidate(_, candidate.steps + 1)).toVector

    @tailrec
    def iterate(candidates: Vector[Candidate]): Int =
      candidates.find(_.molecule == target) match {
        case Some(c) => c.steps
        case None    => iterate(candidates.flatMap(nextCandidates(rules)).sortBy(_.molecule.length).take(1))
      }

    iterate(Vector(Candidate(start, 0)))
  }

  def switchRules(genetics: Genetics, target: String): (Genetics, String) =
    (Genetics(target, genetics.rules.map(rule => Rule(rule.to, rule. from))), genetics.molecule)
}
