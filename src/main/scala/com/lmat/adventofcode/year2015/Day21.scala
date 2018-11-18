package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.adventofcode.year2015.Day21Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day21Definitions {
  case class Character(hitPoints: Int, damage: Int, armor: Int)
  case class Game(shop: Shop, player: Character, boss:Character)

  case class Item(name: String, cost: Int, damage: Int, armor: Int)
  case class Shop(weapons: List[Item], armors: List[Item], rings: List[Item])

  case class ItemSet(weapon: Item, armorItem: Option[Item], leftRing: Option[Item], rightRing: Option[Item]) {
    private val items: Seq[Item] =
      Seq(Some(weapon), armorItem, leftRing, rightRing).flatten

    val cost  : Int = items.map(_.cost).sum
    val damage: Int = items.map(_.damage).sum
    val armor : Int = items.map(_.armor).sum
  }
}

object Day21 extends CommonPuzzle[Character, Game, Int, Int] {
  override def parse(resource: String): Character = parseCharacter(readResource(resource)).get

  def parseCharacter(rows: Seq[String]): Option[Character] = for {
    hitPoints <- rows.headOption.flatMap(parseAttribute(_, "Hit Points: (.*)"))
    damage    <- rows.lift(1).flatMap(parseAttribute(_, "Damage: (.*)"))
    armor     <- rows.lift(2).flatMap(parseAttribute(_, "Armor: (.*)"))
  } yield Character(hitPoints, damage, armor)

  def parseAttribute(row: String, pattern: String): Option[Int] = {
    val regex = pattern.r
    row match {
      case regex(value) => Try(value.toInt).toOption
      case _            => None
    }
  }

  override def preProcess(boss: Character): Game =
    Game(Shop(
      List(
        Item("Dagger",      8, 4, 0),
        Item("Shortsword", 10, 5, 0),
        Item("Warhammer",  25, 6, 0),
        Item("Longsword",  40, 7, 0),
        Item("Greataxe",   74, 8, 0)
      ),
      List(
        Item("Leather",     13, 0, 1),
        Item("Chainmail",   31, 0, 2),
        Item("Splintmail",  53, 0, 3),
        Item("Bandedmail",  75, 0, 4),
        Item("Platemail",  102, 0, 5)
      ),
      List(
        Item("Damage +1",   25, 1, 0),
        Item("Damage +2",   50, 2, 0),
        Item("Damage +3",  100, 3, 0),
        Item("Defense +1",  20, 0, 1),
        Item("Defense +2",  40, 0, 2),
        Item("Defense +3",  80, 0, 3)
      )
    ),
      Character(100, 0, 0),
      boss)

  override def part1(game: Game): Int =
    possibleItemsSets(game.shop)
      .filter(itemSet => canWin(equip(game.player, itemSet), game.boss))
      .map(_.cost).min

  def possibleItemsSets(shop: Shop): Set[ItemSet] = (for{
    weapon      <- shop.weapons
    armor       <- shop.armors.map(Some(_)) :+ None
    leftRing    <- shop.rings.map(Some(_))  :+ None
    rightRing   <- shop.rings.map(Some(_))  :+ None
  } yield ItemSet(weapon, armor, leftRing, rightRing)).toSet

  def equip(player: Character, itemSet: ItemSet): Character =
    Character(player.hitPoints, player.damage + itemSet.damage, player.armor + itemSet.armor)

  def canWin(player: Character, boss: Character): Boolean = {
    @tailrec
    def fight(player: Character, boss: Character, playersTurn: Boolean): Boolean =
      if (player.hitPoints <= 0) false
      else if (boss.hitPoints <= 0) true
      else if(playersTurn) fight(player, damage(player, boss), false)
      else fight(damage(boss, player), boss, true)

    fight(player, boss, true)
  }

  def damage(attacker: Character, defender: Character): Character =
    defender.copy(hitPoints = defender.hitPoints - calculateDamage(attacker, defender))

  def calculateDamage(attacker: Character, defender: Character): Int =
    (attacker.damage - defender.armor) max 1

  override def part2(game: Game): Int =
    possibleItemsSets(game.shop)
      .filter(itemSet => !canWin(equip(game.player, itemSet), game.boss))
      .map(_.cost).max
}
