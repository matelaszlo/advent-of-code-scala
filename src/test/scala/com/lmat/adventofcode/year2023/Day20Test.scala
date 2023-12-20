package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day20._
import com.lmat.adventofcode.year2023.Day20Definitions._
class Day20Test extends AnyFunSuite {
  val raw1 =
    """broadcaster -> a, b, c
      |%a -> b
      |%b -> c
      |%c -> inv
      |&inv -> a""".stripMargin

  val raw2 =
    """broadcaster -> a
      |%a -> inv, con
      |&inv -> b
      |%b -> con
      |&con -> output""".stripMargin

  val machines1Parsed: Machines = Map(
    "broadcaster" -> Broadcaster(List("a", "b", "c")),
    "a"           -> FlipFlop(false, List("b")),
    "b"           -> FlipFlop(false, List("c")),
    "c"           -> FlipFlop(false, List("inv")),
    "inv"         -> Conjuction(Map(), List("a"))
  )

  val machines1Ready: Machines = Map(
    "button"      -> Button,
    "broadcaster" -> Broadcaster(List("a", "b", "c")),
    "a"           -> FlipFlop(false, List("b")),
    "b"           -> FlipFlop(false, List("c")),
    "c"           -> FlipFlop(false, List("inv")),
    "inv"         -> Conjuction(Map("c" -> false), List("a"))
  )

  val machines2Parsed: Machines = Map(
    "broadcaster" -> Broadcaster(List("a")),
    "a"           -> FlipFlop(false, List("inv", "con")),
    "inv"         -> Conjuction(Map(), List("b")),
    "b"           -> FlipFlop(false, List("con")),
    "con"         -> Conjuction(Map(), List("output")),
  )

  val machines2Ready: Machines = Map(
    "button" -> Button,
    "broadcaster" -> Broadcaster(List("a")),
    "a" -> FlipFlop(false, List("inv", "con")),
    "inv" -> Conjuction(Map("a" -> false), List("b")),
    "b" -> FlipFlop(false, List("con")),
    "con" -> Conjuction(Map("a" -> false, "b" -> false), List("output")),
    "output" -> Test
  )

  test("parse") {
    assert(parseMachines(raw1.split("\n").toList) == machines1Parsed)
    assert(parseMachines(raw2.split("\n").toList) == machines2Parsed)
  }

  test("preProcess") {
    assert(preProcess(machines1Parsed) == machines1Ready)
    assert(preProcess(machines2Parsed) == machines2Ready)
  }

  test("buttonPress") {
    val (machines1State, machines1Pulses) = sendPulse(machines1Ready, Pulse("button", "broadcaster", false))
    assert(machines1State == machines1Ready)
    assert(machines1Pulses == List(
      Pulse("button", "broadcaster", false),
      Pulse("broadcaster", "a", false),
      Pulse("broadcaster", "b", false),
      Pulse("broadcaster", "c", false),
      Pulse("a", "b", true),
      Pulse("b", "c", true),
      Pulse("c", "inv", true),
      Pulse("inv", "a", false),
      Pulse("a", "b", false),
      Pulse("b", "c", false),
      Pulse("c", "inv", false),
      Pulse("inv", "a", true)
    ))

    val (machines2State, machines2Pulses) = sendPulses(machines2Ready, List.fill(4)(Pulse("button", "broadcaster", false)))
    assert(machines2State == machines2Ready)
    assert(machines2Pulses == List(
      // First
      Pulse("button", "broadcaster", false),
      Pulse("broadcaster", "a", false),
      Pulse("a", "inv", true),
      Pulse("a", "con", true),
      Pulse("inv", "b", false),
      Pulse("con", "output", true),
      Pulse("b", "con", true),
      Pulse("con", "output", false),
      // Second
      Pulse("button", "broadcaster", false),
      Pulse("broadcaster", "a", false),
      Pulse("a", "inv", false),
      Pulse("a", "con", false),
      Pulse("inv", "b", true),
      Pulse("con", "output", true),
      // Third
      Pulse("button", "broadcaster", false),
      Pulse("broadcaster", "a", false),
      Pulse("a", "inv", true),
      Pulse("a", "con", true),
      Pulse("inv", "b", false),
      Pulse("con", "output", false),
      Pulse("b", "con", false),
      Pulse("con", "output", true),
      // Fourth
      Pulse("button", "broadcaster", false),
      Pulse("broadcaster", "a", false),
      Pulse("a", "inv", false),
      Pulse("a", "con", false),
      Pulse("inv", "b", true),
      Pulse("con", "output", true)
    ))
  }

  test("part1") {
    assert(part1(machines1Ready) == 32000000)
    assert(part1(machines2Ready) == 11687500)
  }
}
