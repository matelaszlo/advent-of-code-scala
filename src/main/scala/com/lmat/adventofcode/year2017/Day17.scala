package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimplePuzzle
import com.lmat.util.Files.readResource

object Day17 extends SimplePuzzle[Int, Int, Int] {
  override def parse(resource: String): Int = readResource(resource).head.toInt

  override def part1(steps: Int): Int = {
    val buffer =
      (0 until 2017)
        .foldLeft((State(0, 0), Seq(0))) { case ((state, b), _) =>
          val next = nextState(state, steps)
          (next, nextBuffer(b, next))
        }._2
    valueAfter(buffer, 2017)
  }

  case class State(position: Int, value: Int)

  def nextState(state: State, numberOfSteps: Int): State = {
    val nextValue = state.value + 1
    val nextPosition = ((state.position + numberOfSteps) % nextValue) + 1
    State(nextPosition, nextValue)
  }

  def nextBuffer(buffer: Seq[Int], nextState: State): Seq[Int] =
    buffer.dropRight(buffer.size - nextState.position) ++ Seq(nextState.value) ++ buffer.drop(nextState.position)

  def valueAfter(buffer: Seq[Int], value: Int): Int =
    buffer.lift(buffer.indexOf(2017) + 1).getOrElse(buffer.head)

  /**
    * Notice that 0 will always stay in position 0
    * We can use this insight to be much lighter on memory allocation and only keep track of the positions and the element in position 1
    */
  override def part2(steps: Int): Int =
    (0 until 50000000)
      .foldLeft((State(0, 0), 0)) { case ((state, positionOne), _) =>
        val next = nextState(state, steps)
        (next, nextPositionOne(positionOne, next))
      }._2

  def nextPositionOne(positionOne: Int, nextState: State): Int =
    if (nextState.position == 1) nextState.value else positionOne
}
