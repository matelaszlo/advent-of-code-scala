package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2017.Day07Definitions._
import com.lmat.util.Files.readResource
import scala.language.postfixOps

object Day07Definitions {
  case class TowerDefinition(name: String, weight: Int, children: Set[String])
}

object Day07 extends SimpleCommonPuzzle[Set[TowerDefinition], String, Int] {

  override def parse(resource: String): Set[TowerDefinition] =
    readResource(resource).flatMap(parseTowerDefinition).toSet

  def parseTowerDefinition(row: String): Option[TowerDefinition] = {
    val simple = "(.*) \\((.*)\\)".r
    val withChildren = "(.*) \\((.*)\\) -> (.*)".r
    row match {
      case withChildren(name, weight, children) => Some(TowerDefinition(name, weight.toInt, children.split(", ").toSet))
      case simple(name, weight) => Some(TowerDefinition(name, weight.toInt, Set()))
      case _ => None
    }
  }

  /**
    * We can get the bottom name cleverly by seeing what name does not appear in the children list
    */
  override def part1(towerDefinitions: Set[TowerDefinition]): String = {
    val allNames = towerDefinitions.map(_.name)
    val excluding = towerDefinitions.flatMap(_.children)
    allNames -- excluding head
  }

  case class Tower(name: String, children: Set[Tower], discWeight: Int) {
    val towerWeight: Int = children.toSeq.map(_.towerWeight).sum + discWeight
  }

  object Tower {
    def apply(name: String, children: Set[Tower], discWeight: Int): Tower =
      new Tower(name, children, discWeight)

    def apply(name: String, discWeight: Int): Tower =
      new Tower(name, Set(), discWeight)
  }

  /**
    * For this part we have to build the Tower
    * We can use the knowledge of what is the bottom tower to simplify the building process greatly
    * Then we can eagerly traverse the Towers looking for the wrongly weighted one
    */
  override def part2(towerDefinitions: Set[TowerDefinition]): Int = {
    val bottom = part1(towerDefinitions)
    val tower = buildTower(towerDefinitions, bottom)
    getCorrectedWeight(tower)
  }

  def buildTower(definitions: Set[TowerDefinition], bottom: String): Tower =
    definitions.find(_.name == bottom).get match {
      case TowerDefinition(name, weight, children) => Tower(name, children.map(childName => buildTower(definitions, childName)), weight)
    }

  def getCorrectedWeight(tower: Tower): Int = {
    def getCorrectWeight(tower: Tower, correctWeight: Int): Int = tower match {
      case Tower(_, children, discWeight) if children.map(_.towerWeight).size == 1 =>
        discWeight + (correctWeight - tower.towerWeight)
      case Tower(_, children, _) =>
        val splitByWeight = children.groupBy(_.towerWeight).toList.sortBy(_._2.size)(Ordering.Int.reverse).splitAt(1)

        val correctWeight = splitByWeight._1.head._1
        val wrongTower = splitByWeight._2.head._2.head

        getCorrectWeight(wrongTower, correctWeight)
      case _ => throw new IllegalArgumentException("Sth went wrong!")
    }
    getCorrectWeight(tower, tower.towerWeight)
  }
}
