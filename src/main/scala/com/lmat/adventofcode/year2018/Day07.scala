package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2018.Day07Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day07Definitions {
  case class Instruction(before: Char, after: Char)
}

object Day07 extends SimpleCommonPuzzle[Seq[Instruction], String, Int] {
  override def parse(resource: String): Seq[Instruction] = readResource(resource).flatMap(parseInstruction)

  def parseInstruction(row: String): Option[Instruction] = {
    val pattern = "Step (.*?) must be finished before step (.*?) can begin.".r
    row match {
      case pattern(before, after) => Some(Instruction(before.toCharArray.head, after.toCharArray.head))
      case _                      => None
    }
  }

  /**
    * Notice that we can decide what step is available by looking at what step does not feature on the right hand side of any remaining instructions
    *
    */
  override def part1(instructions: Seq[Instruction]): String =
    orderSteps(instructions)

  def orderSteps(instructions: Seq[Instruction]): String = {
    @tailrec
    def iterate(toPlace: Set[Char], remaining: Seq[Instruction], built: String): String =
      if(toPlace.isEmpty) built
      else {
        val available = toPlace -- remaining.map(_.after).toSet
        val placed = available.min
        iterate(toPlace - placed, remaining.filterNot(_.before == placed), built + placed)
      }

    iterate(allSteps(instructions), instructions, "")
  }

  def allSteps(instructions: Seq[Instruction]): Set[Char] =
    instructions.map(_.before).toSet ++ instructions.map(_.after).toSet

  override def part2(instructions: Seq[Instruction]): Int =
    completionTime(60, 5)(instructions)

  def completionTime(plus: Int, workers: Int)(instructions: Seq[Instruction]): Int = {
    val timeM = timeMap(plus)

    @tailrec
    def iterate(toPlace: Set[Char], underPlacement:Map[Char, Int], remaining: Seq[Instruction], built: String, second: Int): Int = {
      // Add this line if interested in the progress of the algorithm
      // println(s"Second = $second, To Place = $toPlace, Under Placement = $underPlacement, Built = $built, Remaining = ${remaining.size}")
      if(toPlace.isEmpty && underPlacement.isEmpty) {
        second
      }
      else {
        val finished = underPlacement.filter(_._2 == 1).keys
        val newUnderPlacement = underPlacement.filterNot(_._2 == 1)

        val newBuilt = built + finished.toSeq.sorted.mkString
        val newRemaining = remaining.filterNot(r => finished.toSet.contains(r.before))
        val available = toPlace -- newRemaining.map(_.after).toSet

        if(newUnderPlacement.size == workers || available.isEmpty){
          iterate(toPlace, newUnderPlacement.map{case (c, i) => (c, i - 1)}, newRemaining, newBuilt, second + 1)
        }
        else {
          val emptyPlaces = workers - newUnderPlacement.size

          val placed = available.toSeq.sorted.take(emptyPlaces)
          val placedMap = placed.map(p => (p, timeM(p))).toMap

          iterate(toPlace -- placed, newUnderPlacement.map{case (c, i) => (c, i - 1)} ++ placedMap, newRemaining, newBuilt, second + 1)
        }
      }
    }

    iterate(allSteps(instructions), Map(), instructions, "", -1)
  }

  def timeMap(plus: Int): Map[Char, Int] =
    ('A' to 'Z').zipWithIndex.map{case (c, i) => (c, i + 1 + plus)}.toMap
}
