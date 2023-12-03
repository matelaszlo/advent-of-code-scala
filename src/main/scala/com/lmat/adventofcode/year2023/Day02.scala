package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day02Definitions._
import com.lmat.util.Files.readResource

object Day02Definitions {
  case class CubeSet(red: Int, green: Int, blue: Int)

  case class Game(id: Int, hands: List[CubeSet])
}

object Day02 extends SimpleCommonPuzzle[List[Game], Int, Int] {
  override def parse(resource: String): List[Game] =
    readResource(resource).flatMap(parseGame).toList

  def parseGame(row: String): Option[Game] = {
    val pattern = s"Game (\\d+): (.*)".r

    row match {
      case pattern(idRaw, hands) => idRaw.toIntOption.map(id => Game(id, hands.split("; ").map(parseHand).toList))
      case _ => None
    }
  }

  def parseHand(raw: String): CubeSet = {
    val cubesMap = raw.split(", ").flatMap(cubes => {
      val pattern = s"(\\d+) (.*)".r
      cubes match {
        case pattern(number, colour) => number.toIntOption.map(n => (colour, n))
        case _ => None
      }
    }).toMap

    CubeSet(cubesMap.getOrElse("red", 0), cubesMap.getOrElse("green", 0), cubesMap.getOrElse("blue", 0))
  }

  override def part1(games: List[Game]): Int =
    games.filter(isPossible(CubeSet(12, 13, 14), _)).map(_.id).sum

  def isPossible(bag: CubeSet, game: Game): Boolean =
    game.hands.forall(isPossible(bag, _))

  def isPossible(bag: CubeSet, hand: CubeSet): Boolean =
    bag.red >= hand.red && bag.green >= hand.green && bag.blue >= hand.blue

  override def part2(games: List[Game]): Int =
    games.map(minimalBag).map(power).sum

  def minimalBag(game: Game): CubeSet =
    CubeSet(game.hands.map(_.red).max, game.hands.map(_.green).max, game.hands.map(_.blue).max)

  def power(bag: CubeSet): Int =
    bag.red * bag.green * bag.blue
}
