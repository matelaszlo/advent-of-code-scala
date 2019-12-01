package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2018.Day24Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day24Definitions {
  case class Group(units: Int, hp: Int, attack: Int, attackType: String, initiative: Int, weaknesses: Seq[String], immunity: Seq[String]){
    def power: Int = units * attack
  }

  type GroupConfiguration = Map[(String, Int), Group]
}

object Day24 extends SimpleCommonPuzzle[GroupConfiguration, Int, Int] {
  override def parse(resource: String): GroupConfiguration = parseGroupConfiguration(readResource(resource))

  def parseGroupConfiguration(rows: Seq[String]): GroupConfiguration = {
    val (immuneRows, infectionRows) = rows.span(_.nonEmpty)
    (parseGroups("immune")(immuneRows.drop(1)) ++ parseGroups("infect")(infectionRows.drop(2))).toMap
  }

  def parseGroups(category: String)(rows: Seq[String]): Seq[((String, Int), Group)] =
    rows.zipWithIndex.flatMap { case (row, i) => parseGroup(row).map(group => ((category, i), group)) }

  def parseGroup(row: String): Option[Group] = {
    val pattern1 = s"(.*?) units each with (.*?) hit points with an attack that does (.*?) (.*?) damage at initiative (.*?)".r

    val pattern2 = s"(.*?) units each with (.*?) hit points \\(weak to (.*?)\\) with an attack that does (.*?) (.*?) damage at initiative (.*?)".r
    val pattern3 = s"(.*?) units each with (.*?) hit points \\(immune to (.*?)\\) with an attack that does (.*?) (.*?) damage at initiative (.*?)".r

    val pattern4 = s"(.*?) units each with (.*?) hit points \\(weak to (.*?); immune to (.*?)\\) with an attack that does (.*?) (.*?) damage at initiative (.*?)".r
    val pattern5 = s"(.*?) units each with (.*?) hit points \\(immune to (.*?); weak to (.*?)\\) with an attack that does (.*?) (.*?) damage at initiative (.*?)".r

    row match {
      case pattern5(unitsS, hitPointsS, immunitiesS, weaknessesS, attackS, attackType, initiativeS) =>
        parseGroup(unitsS, hitPointsS, attackS, attackType, initiativeS, weaknessesS.split(", ").toIndexedSeq, immunitiesS.split(", ").toIndexedSeq)
      case pattern4(unitsS, hitPointsS, weaknessesS, immunitiesS, attackS, attackType, initiativeS) =>
        parseGroup(unitsS, hitPointsS, attackS, attackType, initiativeS, weaknessesS.split(", ").toIndexedSeq, immunitiesS.split(", ").toIndexedSeq)
      case pattern3(unitsS, hitPointsS, immunitiesS, attackS, attackType, initiativeS) =>
        parseGroup(unitsS, hitPointsS, attackS, attackType, initiativeS, Seq(), immunitiesS.split(", ").toIndexedSeq)
      case pattern2(unitsS, hitPointsS, weaknessesS, attackS, attackType, initiativeS) =>
        parseGroup(unitsS, hitPointsS, attackS, attackType, initiativeS, weaknessesS.split(", ").toIndexedSeq, Seq())
      case pattern1(unitsS, hitPointsS, attackS, attackType, initiativeS) =>
        parseGroup(unitsS, hitPointsS, attackS, attackType, initiativeS, Seq(), Seq())
      case _ => None
    }
  }

  def parseGroup(unitsS: String, hitPointsS: String, attackS: String, attackType: String, initiativeS: String, weaknesses: Seq[String], immunities: Seq[String]): Option[Group] = for {
    units      <- Try(unitsS.toInt).toOption
    hitPoints  <- Try(hitPointsS.toInt).toOption
    attack     <- Try(attackS.toInt).toOption
    initiative <- Try(initiativeS.toInt).toOption
  } yield Group(units, hitPoints, attack, attackType, initiative, weaknesses, immunities)

  override def part1(start: GroupConfiguration): Int =
    simulate((start, false))._1.values.map(_.units).sum

  type CombatState = (GroupConfiguration, Boolean)

  def simulate(state: CombatState): CombatState =
    LazyList.iterate(state)(fight).find(finished).get

  /**
    * Fighting happens in order of initiative
    * Groups with 0 units cannot attack
    * We have to account for the possibility of draws given the immunities
    */
  def fight(state: CombatState): CombatState = {
    val (configuration, drawFlag) = state
    val targets = targetSelection(configuration)
    val attackers = configuration.toSeq.sortBy(_._2.initiative)(Ordering.Int.reverse).map(_._1)

    @tailrec
      def iterate(remaining: Seq[(String, Int)], current: GroupConfiguration): GroupConfiguration = remaining match {
        case key +: rest  if current.contains(key) => iterate(rest, attack(targets)(current, key, current(key)))
        case _ +: rest => iterate(rest, current)
        case _ => current
      }

    val after = iterate(attackers, configuration)
    if(after == configuration) (after, true)
    else (after, drawFlag)
  }

  /**
    * Target selection happens in order of effective power, initiative
    * Once a target is selected it cannot be selected by another group
    */
  def targetSelection(state: GroupConfiguration): Map[(String, Int), Option[(String, Int)]] = {
    val ordered = state.toSeq.sortBy(s => (s._2.power, s._2.initiative)).reverse

    @tailrec
      def iterate(remaining: Seq[((String, Int), Group)], available: Map[(String, Int), Group], built: Map[(String, Int), Option[(String, Int)]]): Map[(String, Int), Option[(String, Int)]] = remaining match {
        case (key, group) +: rest =>
          choose(available)(key._1, group) match {
            case Some(chosen) => iterate(rest, available.filterNot{case (k, _) => k == chosen}, built.updated(key, Some(chosen)))
            case None => iterate(rest, available, built.updated(key, None))
          }
        case _ => built
      }

    iterate(ordered, state, Map())
  }

  /**
    * Select a target by highest possible damage, largest effective power, highest initiative
    * Important to note if the highest possible damage is 0 no target selection should be made
    */
  def choose(available: Map[(String, Int), Group])(category: String, group: Group): Option[(String, Int)] =
    available
      .filter{case ((k, _), _) => k != category}.toSeq
      .sortBy{case (_, target) => (calculateDamage(group, target), target.power, target.initiative)}.reverse
      .headOption.map(_._1)
      .filter(key => calculateDamage(group, available(key)) > 0)

  def calculateDamage(group: Group, target: Group): Int =
    if(target.immunity.contains(group.attackType)) 0
    else if(target.weaknesses.contains(group.attackType)) group.power * 2
    else group.power

  def attack(targets: Map[(String, Int), Option[(String, Int)]])(state: GroupConfiguration, key: (String, Int), group: Group): GroupConfiguration = targets(key) match {
    case Some(target) =>
      val targetGroup = state(target)
      val damage = calculateDamage(group, targetGroup)
      val healthRemains = (targetGroup.hp * targetGroup.units) - damage
      if(healthRemains <= 0)
        state.filterNot{case (k, _) => k == target}
      else {
        val unitsLost = damage / targetGroup.hp
        state.updated(target, targetGroup.copy(units = targetGroup.units - unitsLost))
      }
    case None => state
  }

  def finished(state: CombatState): Boolean = {
    val (configuration, drawFlag) = state
    configuration.keys.map(_._1).toSet.size == 1 || drawFlag
  }

  override def part2(start: GroupConfiguration): Int = {
    val result = LazyList.from(1).map(boost => {
        val (result, tieFlag) = simulate((boostImmune(start, boost), false))
        val winners = if (tieFlag) "draw" else result.keys.map(_._1).toSet.head
        (result, winners)})
      .find { case (_, winners) => winners == "immune" }
      .map(_._1).get

    result.values.map(_.units).sum
  }

  def boostImmune(state: GroupConfiguration, boost: Int): GroupConfiguration =
    state.map {
      case (("immune", i), group) => (("immune", i), group.copy(attack = group.attack + boost))
      case (k, v) => (k, v)
    }
}
