package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.adventofcode.year2017.Day24Definitions._
import com.lmat.util.Files.readResource

object Day24Definitions {
  case class Component(ports: Seq[Int]) {
    val strength: Int = ports.sum
  }

  object Component {
    def apply(source: String)  : Component = Component(source.split("/").toIndexedSeq.map(string => string.toInt))
    def apply(p1: Int, p2: Int): Component = Component(Seq(p1, p2))
  }

  case class Bridge(components: Seq[Component], startConnection: Int) {
    lazy val strength: Int = components.map(_.strength).sum
    lazy val endConnection: Int = components match {
      case Seq()                => startConnection
      case elem :: Seq()        => elem.ports.find(_ != startConnection).getOrElse(startConnection)
      case _ :+ before :+ last  => last.ports.find(port => !before.ports.contains(port)).getOrElse(last.ports.head)
    }
  }
}

object Day24 extends CommonPuzzle[Seq[Component], Seq[Bridge], Int, Int] {
  override def parse(resource: String): Seq[Component] =
    readResource(resource).map(Component(_)).toVector

  override def preProcess(components: Seq[Component]): Seq[Bridge] =
    buildBridges(initialBridges(components, 0), components)

  override def part1(bridges: Seq[Bridge]): Int =
    bridges.map(_.strength).max

  override def part2(bridges: Seq[Bridge]): Int =
    bridges.maxBy(bridge => (bridge.components.size, bridge.strength)).strength

  def initialBridges(components: Seq[Component], startConnection: Int): Seq[Bridge] =
    components.filter(component => component.ports.contains(startConnection)).map(c => Bridge(Seq(c), startConnection))

  def children(components: Seq[Component])(bridge: Bridge): Seq[Bridge] = {
    val remainingComponents: Set[Component] = components.toSet -- bridge.components.toSet
    val nextOnes = remainingComponents.filter(component => component.ports.contains(bridge.endConnection))
    nextOnes.toSeq.map(c => Bridge(bridge.components :+ c, bridge.startConnection))
  }

  def notFinished(components: Seq[Component])(bridge: Bridge): Boolean = {
    val remainingComponents: Set[Component] = components.toSet -- bridge.components.toSet
    remainingComponents.flatMap(_.ports).contains(bridge.endConnection)
  }

  def buildBridges(initialBridges: Seq[Bridge], components: Seq[Component]): Seq[Bridge] = {
    val finished   = initialBridges.filterNot(notFinished(components))
    val unfinished = initialBridges.filter(notFinished(components))

    if(unfinished.isEmpty) finished
    else finished ++ unfinished ++ buildBridges(unfinished.flatMap(children(components)), components)
  }
}
