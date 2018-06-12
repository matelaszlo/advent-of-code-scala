package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day05 extends SimpleCommonPuzzle[Vector[Int], Int, Int] {
  override def parse(resource: String): Vector[Int] =
    readResource(resource).map(_.toInt).toVector

  override def part1(offsets: Vector[Int]): Int =
    calculateSteps(offsets, next)

  override def part2(offsets: Vector[Int]): Int =
    calculateSteps(offsets, next2)

  case class State(offsets: Vector[Int], position: Int)

  /**
    * Representing the offsets as a Vector is essential here for performance reasons
    * Vectors have (amortized) constant update and random access both of which we do a very high number of times in this puzzle
    */
  def calculateSteps(offsets: Vector[Int], strategy: State => State): Int = {
    @tailrec
    def calculateSteps(state: State, strategy: State => State, steps: Int): Int = {
      if(hasEscaped(state)) steps
      else calculateSteps(strategy(state), strategy, steps + 1)
    }

    calculateSteps(State(offsets, 0), strategy, 0)
  }

  def hasEscaped(state: State): Boolean = state match {
    case State(offsets, position) => position >= offsets.size || position < 0
  }

  def next(state: State): State = state match {
    case State(offsets, position) =>
      val step = offsets(position)
      State(offsets.updated(position, step + 1), position + step)
  }

  def next2(state: State): State = state match {
    case State(offsets, position) =>
      val step = offsets(position)
      State(offsets.updated(position, if (step >= 3) step - 1 else step + 1), position + step)
  }
}
