package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day19Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day19Definitions {
  type Workflows = Map[String, List[Rule]]

  sealed trait Rule
  case class Check(part: String, operation: Operation, number: Int, target: String) extends Rule
  case class Fallback(target: String) extends Rule

  sealed trait Operation
  case object LessThan extends Operation
  case object GreaterThan extends Operation

  type Rating = Map[String, Int]
  type RatingRanges = Map[String, List[Int]]
}

object Day19 extends SimpleCommonPuzzle[(Workflows, List[Rating]), Long, Long] {
  override def parse(resource: String): (Workflows, List[Rating]) =
    parseWorkflowsAndRatings(readResource(resource).toList)

  def parseWorkflowsAndRatings(rows: List[String]): (Workflows, List[Rating]) = {
    val p1 = rows.takeWhile(_.nonEmpty)
    val p2 = rows.dropWhile(_.nonEmpty).drop(1)

    (p1.flatMap(parseWorkflow).toMap, p2.map(parseRating))
  }

  def parseWorkflow(row: String): Option[(String, List[Rule])] = {
    val pattern = s"(.*)\\{(.*)}".r

    row match {
      case pattern(name, rules) => Some(name, rules.split(",").flatMap(parseRule).toList)
      case _ => None
    }
  }

  def parseRule(raw: String): Option[Rule] = {
    val pattern1 = s"(.)<(.*):(.*)".r
    val pattern2 = s"(.)>(.*):(.*)".r

    raw match {
      case pattern1(rating, rawNum, target) => rawNum.toIntOption.map(num => Check(rating, LessThan, num, target))
      case pattern2(rating, rawNum, target) => rawNum.toIntOption.map(num => Check(rating, GreaterThan, num, target))
      case _ => Some(Fallback(raw))
    }
  }

  def parseRating(raw: String): Rating =
    raw.drop(1).dropRight(1).split(",").flatMap(parsePart).toMap

  def parsePart(raw:String): Option[(String, Int)] = {
    val pattern = s"(.)=(.*)".r

    raw match {
      case pattern(name, rawNum) => rawNum.toIntOption.map(num => (name, num))
      case _ => None
    }
  }

  override def part1(input: (Workflows, List[Rating])): Long = {
    val (workflows, ratings) = input
    ratings.map(rating => (rating, simulate(workflows)(rating))).filter { case (_, s) => s.head == "A" }.map { case (r, _) => r.values.sum.toLong }.sum
  }

  def simulate(workflows: Workflows)(rating: Rating): List[String] = {
    def next(steps: List[String]): List[String] =
      workflows.getOrElse(steps.head, List.empty).find(matches(rating)).map(apply).getOrElse(steps.head) :: steps

    LazyList.iterate(List("in"))(next).dropWhile {case c :: _ => !(c == "A" || c == "R") }.head
  }


  def matches(rating: Rating)(rule: Rule): Boolean = rule match {
      case Check(part, LessThan, number, _)    => rating(part) < number
      case Check(part, GreaterThan, number, _) => rating(part) > number
      case _ : Fallback => true
    }

  def apply(rule: Rule): String = rule match {
    case Check(_, _, _, target) => target
    case Fallback(target) => target
  }

  override def part2(input: (Workflows, List[Rating])): Long = {
    val (workflows, _) = input
    val ratingRanges = Map("x" -> (1 to 4000).toList, "m" -> (1 to 4000).toList,"a" -> (1 to 4000).toList,"s" -> (1 to 4000).toList)
    val ratings = simulate2(workflows)(ratingRanges)
    ratings.map(evaluate).sum
  }

  def evaluate(ratingRanges: RatingRanges): Long =
    ratingRanges.values.map(_.size.toLong).product

  def simulate2(workflows: Workflows)(start: RatingRanges): List[RatingRanges] = {
    // We split the ranges based on the rules in order. Finish parts that apply and carry on with the complement. We process until we hit the Fallback at the end.
    def next(range: RatingRanges, node: String): List[(RatingRanges, String)] = {
      val rules = workflows.getOrElse(node, List.empty)
      rules.foldLeft((List.empty[(RatingRanges, String)], range)) { case ((res, c), r) =>
        val (applied, complement, target) = split(c, r)
        ((applied, target) :: res, complement)
      }._1
    }

    def split(range: RatingRanges, rule: Rule): (RatingRanges, RatingRanges, String) = rule match {
      case Check(part, LessThan, number, target) =>
        val (applied, complement) = range(part).partition(_ < number)
        (range.updated(part, applied), range.updated(part, complement), target)
      case Check(part, GreaterThan, number, target) =>
        val (applied, complement) = range(part).partition(_ > number)
        (range.updated(part, applied), range.updated(part, complement), target)
      case Fallback(target) => (range, Map.empty, target)
    }

    @tailrec
    def loop(finished: List[RatingRanges], remaining: List[(RatingRanges, String)]): List[RatingRanges] = {
      println(s"Finished:${finished.size} Remaining:${remaining.size} Current:${remaining.headOption.map { case (r, n) => s"Node:$n ${r.map { case (a, s) => s"$a => ${s.minOption} - ${s.maxOption}" }}" }}")
      remaining match {
        case (range, node) :: rest =>
          if (node == "R") loop(finished, rest)
          else if (node == "A") loop(range :: finished, rest)
          else loop(finished, rest ++ next(range, node))
        case Nil => finished
      }
    }

    loop(List.empty, List((start, "in")))
  }
}
