package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2018.Day18Definitions._
import com.lmat.util.Files.readResource

object Day18Definitions {
  case class Coordinate(x: Int, y: Int)

  sealed trait Acre
  case object Ground      extends Acre
  case object Tree        extends Acre
  case object LumberyYard extends Acre

  type State = Map[Coordinate, Acre]
}

object Day18 extends SimpleCommonPuzzle[State, Int, Int]{
  override def parse(resource: String): State = parse(readResource(resource))

  def parse(rows: Seq[String]): State =
    rows.zipWithIndex.flatMap { case (r, y) => parse(r, y) }.toMap

  def parse(row: String, y: Int): List[(Coordinate, Acre)] =
    row.toCharArray.zipWithIndex.flatMap { case (c, x) => parse(c).map((Coordinate(x, y), _)) }.toList

  def parse(c: Char): Option[Acre] = c match {
    case '.' => Some(Ground)
    case '|' => Some(Tree)
    case '#' => Some(LumberyYard)
    case _   => None
  }

  def next(state: State): State =
    state.map {case (c, a) => next(state, c, a) }

  def next(state: State, coordinate: Coordinate, acre: Acre): (Coordinate, Acre) = {
    val neighbouringAcres: List[Acre] = neighbours(coordinate).flatMap(state.get)
    val newAcre = acre match {
      case Ground      => if(neighbouringAcres.count(_ == Tree) >= 3) Tree else Ground
      case Tree        => if(neighbouringAcres.count(_ == LumberyYard) >= 3) LumberyYard else Tree
      case LumberyYard => if(neighbouringAcres.count(_ == LumberyYard) >= 1 && neighbouringAcres.count(_ == Tree) >= 1) LumberyYard else Ground
    }
    (coordinate, newAcre)
  }

  def neighbours(c: Coordinate): List[Coordinate] = {
    val Coordinate(x, y) = c
    (for{
      xD <- - 1 to + 1
      yD <- - 1 to + 1
      if !(xD == 0 && yD == 0)
    } yield Coordinate(x + xD, y + yD)).toList
  }

  override def part1(state: State): Int = {
    val simulated = simulate(state).drop(10).head
    println(s"State:\n${print(simulated)}\n")
    count(simulated, Tree) * count(simulated, LumberyYard)
  }

  def simulate(initial: State): LazyList[State] =
    LazyList.iterate(initial)(next)

  def count(state: State, acre: Acre): Int =
    state.count { case (_, a) => a == acre }

  // The largeness of the number in part 2 suggests that we have to think about a clever solution rather than trying to calculate it directly
  // Indeed the problem can be simplified significantly as it quickly turns into a cycle
  override def part2(state: State): Int = {
    val n = 1000000000
    val (start, end) = findCycle(state)
    val size = end - start
    val offset = (n - start) % size
    val element = start + offset
    println(s"Cycle starts at $start and ends at $end and has a size off $size elements")
    println(s"The ${n}th element will match with the ${element}th because it has the same offset in the cycle: $offset")

    val result = simulate(state).drop(element).head
    println(s"State:\n${print(result)}\n")
    count(result, Tree) * count(result, LumberyYard)
  }

  // We can find cycles in the infinite stream of states by
  // By scanning over the state history
  // And finding the first instance that has a duplicate
  def findCycle(initial: State): (Int, Int) = {
    val states = simulate(initial).scanLeft[List[State]](List())((all, s) => s :: all).dropWhile(l => l.size == l.distinct.size).head
    val target = states.head
    (states.reverse.indexOf(target), states.size - 1)
  }

  def print(state: State): String = {
    val xMin = state.keys.map(_.x).min
    val xMax = state.keys.map(_.x).max
    val yMin = state.keys.map(_.y).min
    val yMax = state.keys.map(_.y).max
    (yMin to yMax).map(y => (xMin to xMax).map(x => print(state(Coordinate(x, y)))).mkString("")).mkString("\n")
  }

  def print(acre: Acre): String = acre match {
    case Ground      => "."
    case Tree        => "|"
    case LumberyYard => "#"
  }
}
