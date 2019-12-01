package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day17.{State, nextBuffer, nextState}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day17Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val testStates = Seq(
    (State(0, 0), List(0)),
    (State(1, 1), List(0, 1)),
    (State(1, 2), List(0, 2, 1)),
    (State(2, 3), List(0, 2, 3, 1)),
    (State(2, 4), List(0, 2, 4, 3, 1)),
    (State(1, 5), List(0, 5, 2, 4, 3, 1)),
    (State(5, 6), List(0, 5, 2, 4, 3, 6, 1)),
    (State(2, 7), List(0, 5, 7, 2, 4, 3, 6, 1)),
    (State(6, 8), List(0, 5, 7, 2, 4, 3, 8, 6, 1)),
    (State(1, 9), List(0, 9, 5, 7, 2, 4, 3, 8, 6, 1)))

  test("Test Spinlock states") {
    val states = (0 until 9).scanLeft((State(0, 0), Seq(0))){ case ((state, buffer), _) =>
      val next = nextState(state, 3)
      (next, nextBuffer(buffer, next))
    }

    assert(states == testStates)
  }
}
