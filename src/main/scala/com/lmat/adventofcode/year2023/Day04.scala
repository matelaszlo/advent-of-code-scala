package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day04Definitions._
import com.lmat.util.Files.readResource

object Day04Definitions {
  case class Game(id: Int, winning: List[Int], bet: List[Int])
}

object Day04 extends SimpleCommonPuzzle[List[Game], Int, Int] {
  override def parse(resource: String): List[Game] = readResource(resource).flatMap(parseGame).toList

  def parseGame(row: String): Option[Game] = {
    val pattern = s"Card[ ]*(\\d+): (.*) \\| (.*)".r

    def numbers(raw: String): List[Int] =
      raw.split(" ").flatMap(_.toIntOption).toList

    row match {
      case pattern(rawId, rawWinning, rawBet) => rawId.toIntOption.map(id => Game(id, numbers(rawWinning), numbers(rawBet)))
      case _ => None
    }
  }


  override def part1(games: List[Game]): Int =
    games.map(score).sum

  def score(game: Game): Int =
    Math.pow(2, game.winning.count(game.bet.contains) - 1).toInt

  case class State(map: Map[Int, Game], finished: Map[Int, Int], evaluate: Map[Int, Int])

  override def part2(games: List[Game]): Int = simulate(start(games)).finished.values.sum

  def simulate(start: State): State = LazyList.iterate(start)(step).dropWhile(_.evaluate.nonEmpty).head

  def start(games: List[Game]): State = {
    val gamesMap = games.groupBy(_.id).map { case (k, v) => (k, v.head) }
    State(gamesMap, Map.empty, gamesMap.map { case (k, _) => (k, 1) })
  }

  def step(state: State): State = {
    def copies(id: Int, count: Int): List[(Int, Int)] = {
      val game = state.map(id)
      val n = game.winning.count(game.bet.contains)
      (1 to n).map(_ + game.id).filter(state.map.contains).map(i => (i, count)).toList
    }

    if (state.evaluate.isEmpty) state
    else {
      val min = state.evaluate.keySet.min
      val count = state.evaluate(min)
      val next = copies(min, count).foldLeft(state.evaluate.removed(min)) { case (map, (k, v)) => map.updated(k, map(k) + v) }

      State(state.map, state.finished.updated(min, count), next)
    }
  }
}
