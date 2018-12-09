package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2018.Day09Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Sequences.{shiftRight, shiftLeft}

import scala.util.Try

object Day09Definitions {
  case class GameDefinition(players: Int, lastMarble: Int)
}

object Day09 extends SimpleCommonPuzzle[GameDefinition, Long, Long] {
  override def parse(resource: String): GameDefinition =
    readResource(resource).headOption.flatMap(parseGameDefinition).get

  def parseGameDefinition(row: String): Option[GameDefinition] = {
    val pattern = "(.*?) players; last marble is worth (.*?) points".r
    row match {
      case pattern(playersS, lastMarbleS) => (for {
        players    <- Try(playersS.toInt)
        lastMarble <- Try(lastMarbleS.toInt)
      } yield GameDefinition(players, lastMarble)).toOption
      case _ => None
    }
  }

  case class GameState(marbles: Vector[Int], playerScores: Map[Int, Long], currentPlayer: Int)

  override def part1(gameDefinition: GameDefinition): Long =
    playGame(gameDefinition).playerScores.values.max

  def playGame(gameDefinition: GameDefinition): GameState =
    (1 to gameDefinition.lastMarble).foldLeft(GameState(Vector(0), Map(), 1))(playTurn(gameDefinition))

  /**
    * For efficiency we can rotate the current insertion/deletion point to the head of the sequence and keep it always there
    */
  def playTurn(gameDefinition: GameDefinition)(gameState: GameState, marble: Int): GameState = {
    val nextPlayer = if (gameState.currentPlayer == gameDefinition.players) 1 else gameState.currentPlayer + 1

    if (marble % 23 == 0) {
      val shifted = shiftRight(gameState.marbles, 7).toVector
      val currentPlayerScore = gameState.playerScores.getOrElse(gameState.currentPlayer, 0L)
      val nextPlayerScores = gameState.playerScores.updated(gameState.currentPlayer, currentPlayerScore + marble + shifted.head)
      GameState(shifted.drop(1), nextPlayerScores, nextPlayer)
    } else {
      val shifted = shiftLeft(gameState.marbles, 2).toVector
      GameState(marble +: shifted, gameState.playerScores, nextPlayer)
    }
  }

  override def part2(gameDefinition: GameDefinition): Long =
    playGame(gameDefinition.copy(lastMarble = gameDefinition.lastMarble * 100)).playerScores.values.max
}
