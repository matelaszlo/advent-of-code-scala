package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.{CommonPuzzle, SimpleCommonPuzzle}
import com.lmat.adventofcode.year2023.Day20Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Maths

import scala.annotation.tailrec

object Day20Definitions {
  case class Pulse(from: String, to: String, high: Boolean)

  sealed trait Module
  case object Button extends Module
  case class Broadcaster(targets: List[String]) extends Module
  case class FlipFlop(on: Boolean, targets: List[String]) extends Module
  case class Conjuction(inputs: Map[String, Boolean], targets: List[String]) extends Module
  case object Test extends Module

  type Machines = Map[String, Module]
}
object Day20 extends CommonPuzzle[Machines, Machines, Long, Long]{
  override def parse(resource: String): Machines =
    parseMachines(readResource(resource).toList)

  def parseMachines(rows: List[String]): Machines = {
    def parseRow(row: String): Option[(String, Module)] = {
      val pattern1 = s"broadcaster -> (.*)".r
      val pattern2 = s"%(.*) -> (.*)".r
      val pattern3 = s"&(.*) -> (.*)".r

      row match {
        case pattern1(targets) => Some(("broadcaster", Broadcaster(targets.split(", ").toList)))
        case pattern2(name, targets) => Some(name, FlipFlop(false, targets.split(", ").toList))
        case pattern3(name, targets) => Some(name, Conjuction(Map.empty, targets.split(", ").toList))
        case _ => None
      }
    }

    rows.flatMap(parseRow).toMap
  }

  override def preProcess(parsed: Machines): Machines =
    parsed.toList.flatMap {
      case (name, Broadcaster(targets)) => targets.map((name, _))
      case (name, FlipFlop(_, targets)) => targets.map((name, _))
      case (name, Conjuction(_, targets)) => targets.map((name, _))
      case _ => List.empty
    }.foldLeft(parsed.updated("button", Button)){case (modules, (from, to)) =>
      val module = modules.get(to).map {
        case Conjuction(inputs, targets) => Conjuction(inputs.updated(from, false), targets)
        case m => m
      }.getOrElse(Test)

      modules.updated(to, module)
    }

  override def part1(machines: Machines): Long = {
    val pulses = List.fill(1000)(Pulse("button", "broadcaster", false))
    val (state, sent) = sendPulses(machines, pulses)
    val high = sent.count(_.high)
    val low = sent.size - high
    println(s"Final state:$state\nPulses:${pulses.size} High:$high Low:$low")
    high.toLong * low.toLong
  }

  def sendPulses(machines: Machines, pulses: List[Pulse]): (Machines, List[Pulse]) =
    pulses.foldLeft((machines, List.empty[Pulse])){case ((ms, ps), p) =>
     val (newM, newPs) = sendPulse(ms, p)
      (newM, ps ++ newPs)
    }

  // The return includes all pulses sent in order to be able to test the logic
  def sendPulse(machines: Machines, pulse: Pulse): (Machines, List[Pulse]) = {
    @tailrec
    def loop(machines: Machines, sent: List[Pulse], remaining: List[Pulse]): (Machines, List[Pulse]) =
      remaining match {
        case pulse :: rest =>
          val (updatedMachines, newPulses) = next(machines)(pulse)
          loop(updatedMachines, sent :+ pulse, rest ++ newPulses)
        case Nil => (machines, sent)
      }

    loop(machines, List.empty, List(pulse))
  }

  // The next state and new pulses generated on a pulse evaluation
  def next(machines: Machines)(pulse: Pulse): (Machines, List[Pulse]) = {
    val Pulse(from, to, high) = pulse
    machines(to) match {
      case Button => (machines, List(Pulse("button", "broadcaster", false)))
      case Broadcaster(targets) => (machines, targets.map(t => Pulse("broadcaster", t, high)))
      case FlipFlop(on, targets) =>
        if (high) (machines, List())
        else (machines.updated(to, FlipFlop(!on, targets)), targets.map(t => Pulse(to, t, !on)))
      case Conjuction(inputs, targets) =>
        val updatedInputs = inputs.updated(from, high)
        if (updatedInputs.values.forall(identity)) (machines.updated(to, Conjuction(updatedInputs, targets)), targets.map(t => Pulse(to, t, false)))
        else (machines.updated(to, Conjuction(updatedInputs, targets)), targets.map(t => Pulse(to, t, true)))
      case Test => (machines, List())
    }
  }

  // Simulating until we get a pulse does not work so wee need to simplify based on the peculiarities of the input
  // Only one Conjuction node leads to rx
  // That is fed by a list of also Conjuction nodes
  // We check when each of these nodes send a high pulse assume and calculate LCM
  // This only works as the inputs are fabricated in such a way that cycles are independent and start from the beginning
  override def part2(machines: Machines): Long = {
    val conjunction = findConjunctionsTargeting(machines, "rx").head
    val cycles = findConjunctionsTargeting(machines, conjunction).map(n => (n, findCycle(machines, n)))

    println("Cycles:")
    cycles.foreach { case (n, l) => println(s"$n: $l") }

    cycles.map(_._2).reduce(Maths.lcm)
  }

  def findCycle(machines: Machines, target: String): Long =
    LazyList.iterate((machines, List.empty[Pulse])) { case (m, _) => sendPulse(m, Pulse("button", "broadcaster", false)) }.zipWithIndex
      .find { case ((_, p), _) => p.exists(p => p.from == target && p.high) }.map(_._2).getOrElse(0).toLong

  def findConjunctionsTargeting(machines: Machines, target: String): List[String] =
    machines.filter {
      case (_, Conjuction(_, t)) => t.contains(target)
      case _ => false
    }.keys.toList
}
