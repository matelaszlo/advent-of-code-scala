package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.adventofcode.year2015.Day22Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Search.streamSearch

import scala.util.Try

object Day22Definitions {
  case class Warrior(hitPoints: Int, damage: Int, armor: Int)
  case class Wizard(hitPoints: Int, mana: Int, armor: Int)

  case class Game(spells: Set[Spell], player: Wizard, boss:Warrior)

  sealed trait Spell {
    def cost: Int
  }
  case class MagicMissile(cost: Int, damage: Int)       extends Spell
  case class Drain(cost: Int, amount: Int)              extends Spell
  case class Shield(cost: Int, armor: Int, turns: Int)  extends Spell
  case class Poison(cost: Int, damage: Int, turns: Int) extends Spell
  case class Recharge(cost: Int, mana: Int, turns: Int) extends Spell

  sealed trait Effect
  case class ArmorUp(value: Int, turns: Int) extends Effect
  case class Damage(value: Int, turns: Int)  extends Effect
  case class Mana(value: Int, turns: Int)    extends Effect
}

object Day22 extends CommonPuzzle[Warrior, Game, Int, Int] {
  override def parse(resource: String): Warrior = parseWarrior(readResource(resource)).get

  def parseWarrior(rows: Seq[String]): Option[Warrior] = for {
    hitPoints <- rows.headOption.flatMap(parseAttribute(_, "Hit Points: (.*)"))
    damage    <- rows.lift(1).flatMap(parseAttribute(_, "Damage: (.*)"))
    armor     <- Some(0)
  } yield Warrior(hitPoints, damage, armor)

  def parseAttribute(row: String, pattern: String): Option[Int] = {
    val regex = pattern.r
    row match {
      case regex(value) => Try(value.toInt).toOption
      case _            => None
    }
  }

  override def preProcess(boss: Warrior): Game =
    Game(
      Set(
       MagicMissile(53, 4),
        Drain(73, 2),
        Shield(113, 7 ,6),
        Poison(173, 3, 6),
        Recharge(229, 101, 5),
      ),
      Wizard(50, 500, 0),
      boss)

  case class GameState(player: Wizard, boss: Warrior, effects: Set[Effect])
  case class SearchState(gameState: GameState, playersTurn: Boolean, manaUsed: Int, spellsUsed: Seq[Spell])

  override def part1(game: Game): Int = {
    def children(searchState: SearchState): LazyList[SearchState] =
      if (isFinished(searchState.gameState)) LazyList()
      else {
        val afterEffects = applyEffects(searchState.gameState)
        if (searchState.playersTurn)
          game.spells.filter(_.cost <= afterEffects.player.mana).filter(isCastable(_, afterEffects.effects))
            .to(LazyList).sortBy(_.cost)
            .map(spell => (spell, useSpell(afterEffects.player, afterEffects.boss, spell)))
            .flatMap { case (spell, nextState) => LazyList(SearchState(nextState.copy(effects = nextState.effects ++ afterEffects.effects), false, searchState.manaUsed + spell.cost, searchState.spellsUsed :+ spell)) }
        else
          LazyList(SearchState(afterEffects.copy(player = damage(afterEffects.boss, afterEffects.player)), true, searchState.manaUsed, searchState.spellsUsed))
      }

    streamSearch(LazyList(SearchState(GameState(game.player, game.boss, Set()), true, 0, Seq())), children)
      .filter(s => isWon(s.gameState)).head.manaUsed
  }

  def isCastable(spell: Spell, activeEffects: Set[Effect]): Boolean = spell match {
    case Shield(_, _, _)   => !activeEffects.exists(_.isInstanceOf[ArmorUp])
    case Poison(_, _, _)   => !activeEffects.exists(_.isInstanceOf[Damage])
    case Recharge(_, _, _) => !activeEffects.exists(_.isInstanceOf[Mana])
    case _                 => true
  }

  def isWon(gameState: GameState): Boolean =
    gameState.player.hitPoints > 0 && gameState.boss.hitPoints <= 0

  def isFinished(gameState: GameState): Boolean =
    gameState.player.hitPoints <= 0 || gameState.boss.hitPoints <= 0

  def applyEffects(gameState: GameState): GameState = {
    def applyEffect(player: Wizard, boss: Warrior, effect: Effect): (Wizard, Warrior) = effect match {
      case ArmorUp(value, turns) if turns == 1 => (player.copy(armor = player.armor - value), boss)
      case Mana(value, _)                      => (player.copy(mana  = player.mana + value) , boss)
      case Damage(value, _)                    => (player, boss.copy(hitPoints = boss.hitPoints - value))
      case _                                   => (player, boss)
    }

    def nextEffect(effect: Effect): Option[Effect] = effect match {
      case ArmorUp(value, turns) => if(turns == 1) None else Some(ArmorUp(value, turns - 1))
      case Mana(value, turns)    => if(turns == 1) None else Some(Mana(value, turns - 1))
      case Damage(value, turns)  => if(turns == 1) None else Some(Damage(value, turns - 1))
    }

    val (player, boss) = gameState.effects.foldLeft((gameState.player, gameState.boss)){case ((p, b), e) => applyEffect(p, b, e)}
    val nextEffects = gameState.effects.flatMap(e => nextEffect(e))
    GameState(player, boss, nextEffects)
  }

  def damage(warrior: Warrior, wizard: Wizard): Wizard =
    wizard.copy(hitPoints = wizard.hitPoints - ((warrior.damage - wizard.armor) max 1))

  def useSpell(wizard: Wizard, warrior: Warrior, spell: Spell): GameState = spell match {
    case MagicMissile(cost, damage) => GameState(
      wizard.copy(mana = wizard.mana - cost),
      warrior.copy(hitPoints = warrior.hitPoints - damage),
      Set())
    case Drain(cost, amount)        => GameState(
      wizard.copy(mana = wizard.mana - cost, hitPoints = wizard.hitPoints + amount),
      warrior.copy(hitPoints = warrior.hitPoints - amount),
      Set())
    case Shield(cost, armor, turns) => GameState(
      wizard.copy(mana = wizard.mana - cost, armor = wizard.armor + armor),
      warrior,
      Set(ArmorUp(armor, turns)))
    case Poison(cost, damage, turns) => GameState(
      wizard.copy(mana = wizard.mana - cost),
      warrior,
      Set(Damage(damage, turns)))
    case Recharge(cost, mana, turns) => GameState(
      wizard.copy(mana = wizard.mana - cost),
      warrior,
      Set(Mana(mana, turns)))
  }

  override def part2(game: Game): Int = {
    def children(searchState: SearchState): LazyList[SearchState] =
      if (isFinished(searchState.gameState)) LazyList()
      else {
        if (searchState.playersTurn) {
          val afterEffects = applyEffects(searchState.gameState.copy(player = searchState.gameState.player.copy(hitPoints = searchState.gameState.player.hitPoints - 1)))
          game.spells.filter(_.cost <= afterEffects.player.mana).filter(isCastable(_, afterEffects.effects))
            .to(LazyList).sortBy(_.cost)
            .map(spell => (spell, useSpell(afterEffects.player, afterEffects.boss, spell)))
            .flatMap { case (spell, nextState) => LazyList(SearchState(nextState.copy(effects = nextState.effects ++ afterEffects.effects), false, searchState.manaUsed + spell.cost, searchState.spellsUsed :+ spell)) }
        }
        else {
          val afterEffects = applyEffects(searchState.gameState)
          LazyList(SearchState(afterEffects.copy(player = damage(afterEffects.boss, afterEffects.player)), true, searchState.manaUsed, searchState.spellsUsed))
        }
      }

    streamSearch(LazyList(SearchState(GameState(game.player, game.boss, Set()), true, 0, Seq())), children)
      .filter(s => isWon(s.gameState)).head.manaUsed
  }
}
