package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day12Definitions._
import com.lmat.util.Files.readResource

object Day12Definitions {
  type Springs = (List[Condition], List[Int])

  sealed trait Condition
  case object Operational extends Condition
  case object Damaged extends Condition
  case object Unknown extends Condition
}

object Day12 extends SimpleCommonPuzzle[List[Springs], Long, Long] {
  override def parse(resource: String): List[Springs] =
    readResource(resource).toList.map(parseSprings)

  def parseSprings(row: String): Springs = {
    val s = row.split(" ").toList
    (s.head.toCharArray.toList.map(parseCondition), s(1).split(",").toList.flatMap(_.toIntOption))
  }

  def parseCondition(c: Char): Condition = c match {
    case '#' => Damaged
    case '.' => Operational
    case _ => Unknown
  }

  def print(springs: Springs): String = {
    val (conditions, numbers) = springs
    val cString = conditions.map {
      case Operational => '.'
      case Damaged => '#'
      case Unknown => '?'
    }.mkString("")
    val nString = numbers.mkString(",")
    s"$cString $nString"
  }

  override def part1(springs: List[Springs]): Long =
    springs.map(arrangements).sum

  override def part2(springs: List[Springs]): Long =
    springs.map(expand(5)).map(arrangements).sum

  def expand(n: Int)(springs: Springs): Springs = {
    val (conditions, numbers) = springs
    (List.fill(n)(conditions.appended(Unknown)).flatten.dropRight(1), List.fill(n)(numbers).flatten)
  }

  def arrangements(springs: Springs): Long = {
    type Cache = Map[(String, Int), Long]

    def loop(conditions: List[Condition], numbers: List[Int], space: Int, cache: Cache): (Long, Cache) = {
      val key = (print((conditions, numbers)), space)
//      println(s"Looping ${print((conditions, numbers))} Space:$space Cache Size:${cache.size} ")

      cache.get(key) match {
        case Some(value) => (value, cache)
        case None =>
          val (result, updatedCache) = numbers match {
            case first :: rest =>
              // We try to place the first elements in as many ways as we can based off the maximum available space
              (0 to space).filter(i => {
                  // We check if we can place the first element in the position starting with i
                  val before = !conditions.take(i).contains(Damaged)                                       // Elements before are all non damaged
                  val placement = conditions.slice(i, i + first).forall(c => c == Damaged || c == Unknown) // Where we want to place it all elements are unknown or damaged
                  val after = !conditions.drop(i + first).headOption.contains(Damaged)                     // Element after if exists is not damaged
                  before && placement && after
                })
                .foldLeft((0L, cache)) { case ((sum, cache), i) =>
                  // We recursively solve the rest and carry the cache
                  val (res, newCache) = loop(conditions.drop(i + first + 1), rest, space - i, cache)
                  (sum + res, newCache)
                }
            case _ =>
              // If we no longer have numbers to place we cannot have damaged spaces to fill anymore
              val res = if (!conditions.contains(Damaged)) 1L else 0L
              (res, cache)
          }
          (result, updatedCache.updated(key, result))
      }
    }
    val (conditions, numbers) = springs
    val (result, finalCache) = loop(conditions, numbers, conditions.size - (numbers.sum + numbers.size - 1), Map.empty)
    println(s"${print(springs)} => $result | Final cache size:${finalCache.size}")
    result
  }
}
