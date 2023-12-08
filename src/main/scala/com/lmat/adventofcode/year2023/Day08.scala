package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day08Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Maths

object Day08Definitions {
  case class Network(steps: List[Direction], nodes: Map[String, Map[Direction, String]])

  sealed trait Direction

  case object Left extends Direction

  case object Right extends Direction

}

object Day08 extends SimpleCommonPuzzle[Network, Long, Long] {
  override def parse(resource: String): Network =
    parseNetwork(readResource(resource).toList)

  def parseNetwork(rows: List[String]): Network = {
    def directions(left: String, right: String): Map[Direction, String] =
      Map(Left -> left, Right -> right)

    val steps = rows.head.toCharArray.flatMap(parseDirection).toList
    val nodes = rows.drop(2).flatMap(parseNode).groupBy(_._1).map { case (k, v) => (k, directions(v.head._2, v.head._3)) }
    Network(steps, nodes)
  }

  def parseDirection(c: Char): Option[Direction] = c match {
    case 'R' => Some(Right)
    case 'L' => Some(Left)
    case _ => None
  }

  def parseNode(row: String): Option[(String, String, String)] = {
    val pattern = s"(.*) = \\((.*), (.*)\\).*".r
    row match {
      case pattern(current, left, right) => Some(current, left, right)
      case _ => None
    }
  }

  case class State(currentNode: String, currentStep: Int, total: Long)

  override def part1(network: Network): Long = simulate(network)("AAA", _ == "ZZZ")

  override def part2(network: Network): Long = {
    val simulations = network.nodes.keySet.filter(_.endsWith("A")).map(start => (start, simulate(network)(start, _.endsWith("Z"))))
    println(simulations)
    simulations.map(_._2).reduce(Maths.lcm)
  }

  def simulate(network: Network)(start: String, isEnd: String => Boolean): Long =
    LazyList.iterate(State(start, 0, 0))(next(network)).find(state => isEnd(state.currentNode)).map(_.total).getOrElse(Long.MaxValue)

  def next(network: Network)(state: State): State = {
    val nextNode = network.nodes(state.currentNode)(network.steps(state.currentStep))
    val nextStep = (state.currentStep + 1) % network.steps.size
    val total = state.total + 1
    State(nextNode, nextStep, total)
  }
}
