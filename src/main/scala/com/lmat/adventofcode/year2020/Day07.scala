package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day07Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day07Definitions {
  type Bag = String
  type BagRules = Map[Bag, Map[Bag, Int]]
}

object Day07 extends SimpleCommonPuzzle[BagRules, Int, Int]{
  override def parse(resource: String): BagRules =
    parseBagRules(readResource(resource).mkString("\n"))

  def parseBagRules(source: String): BagRules =
    source.split("\n").flatMap(parseBagRule).toMap

  def parseBagRule(row: String): Option[(String, Map[String, Int])] = {

    def parseBag(raw: String): Option[Bag] = {
      val pattern1 = s"(.*) bag".r
      val pattern2 = s"(.*) bags".r

      raw match {
        case pattern1(bag) => Some(bag)
        case pattern2(bag) => Some(bag)
        case _ => None
      }
    }

    def parseComponent(raw: String): Option[(Bag, Int)] = {
      val pattern = s"(\\d+) (.*)".r
      raw match {
        case pattern(countRaw, bagRaw) => for {
          count <- Try(countRaw.toInt).toOption
          bag <- parseBag(bagRaw)
        } yield (bag, count)
        case _ => None
      }
    }

    def parseRight(right: String): Map[Bag, Int] =
      right.split(", ").flatMap(parseComponent).toMap

    val components: List[String] = row.dropRight(1).split(" contain ").toList
    components match {
      case List(leftRaw, rightRaw) => parseBag(leftRaw).map(left => (left, parseRight(rightRaw)))
      case _ => None
    }
  }

  override def part1(bagRules: BagRules): Int =
    findPossibleContainers(bagRules)("shiny gold").size

  override def part2(bagRules: BagRules): Int =
    buildBagCountMap(bagRules)("shiny gold")

  /**
    * Algorithm:
    * Given a Set of target bags find the Set of bags that contain any
    * Recursively do this until the Set stops expanding
    * Remove the original target bag
    */
  def findPossibleContainers(bagRules: BagRules)(target: Bag): Set[Bag] = {
    @tailrec
    def loop(current: Set[Bag]): Set[Bag] = {
//      println(s"Current: $current")
      val newBags = bagRules.filter{case (_,containedBags) => containedBags.exists{case (bag, _) => current.contains(bag)}}.keySet
      if ((newBags -- current).isEmpty) current
      else loop(newBags ++ current)
    }
    loop(Set(target)) - target
  }

  /**
    * Algorithm:
    * Given the bag rules find all the trivially solvable cases
    * - A case is trivially solvable if all bags it contains have already been solved
    * Add their solution to the solutions map and remove them from the unsolved cases
    * Recursively do this until there is no more solvable cases or no more unsolved cases remain
    */
  def buildBagCountMap(bagRules: BagRules): Map[Bag, Int] = {
    @tailrec
    def loop(remaining: BagRules, current: Map[Bag, Int]): Map[Bag, Int] = {
      val triviallySolvable = remaining.filter{case (_,containedBags) => containedBags.forall{case (bag, _ ) => current.contains(bag)}}
//      println(s"\nCurrent: $current")
//      println(s"Remaining: ${remaining.keySet}")
//      println(s"TriviallySolvable: ${triviallySolvable.keySet}")
      val nextState = triviallySolvable.foldLeft(current){case (state, (bag, containedBags)) => state.updated(bag, containedBags.map {case (containedBag, count) => count + current(containedBag) * count}.sum)}
      val nextRemaining = remaining.removedAll(triviallySolvable.keySet)

      if(nextRemaining.isEmpty || triviallySolvable.isEmpty) nextState
      else loop(nextRemaining, nextState)
    }

    loop(bagRules, Map())
  }
}
