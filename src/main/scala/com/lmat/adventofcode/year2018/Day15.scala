package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2018.Day15Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day15Definitions {
  sealed trait Tile
  case object  Empty                        extends Tile
  case object  Wall                         extends Tile
  sealed trait Character                    extends Tile {
    def attack: Int
    def hp: Int
  }
  case class   Goblin(attack: Int, hp: Int) extends Character
  case class   Elf(attack: Int, hp: Int)    extends Character

  type Coordinates = (Int, Int)

  case class CombatMap(map: Map[Coordinates, Tile]) {
    val elves:      Seq[(Coordinates, Elf)]       = map.collect { case (c, elf@Elf(_, _)) => (c, elf) }.toSeq
    val goblins:    Seq[(Coordinates, Goblin)]    = map.collect { case (c, goblin@Goblin(_, _)) => (c, goblin) }.toSeq
    val characters: Seq[(Coordinates, Character)] = (elves ++ goblins).sortBy{case ((x, y), _) => (y, x)}
  }

  type CombatState = (CombatMap, Int, Boolean)
}

object Day15 extends SimpleCommonPuzzle[CombatMap, Int, Int] {
  override def parse(resource: String): CombatMap = parseCombatState(readResource(resource))

  def parseCombatState(string: String): CombatMap =
    parseCombatState(string.split("\n").toIndexedSeq)

  def parseCombatState(rows: Seq[String]): CombatMap = {
    def parseRow(rowIndex: Int)(row: String): Seq[(Coordinates, Tile)] =
      row.zipWithIndex.map { case (char, columnIndex) => ((columnIndex, rowIndex), parseTile(char)) }

    def parseTile(char: Char): Tile = char match {
      case '#' => Wall
      case '.' => Empty
      case 'G' => Goblin(3, 200)
      case 'E' => Elf(3, 200)
      case _   => throw new IllegalArgumentException(s"Cannot parse tile: $char")
    }

    CombatMap(rows.zipWithIndex.flatMap { case (row, index) => parseRow(index)(row) }.toMap)
  }

  override def part1(combatMap: CombatMap): Int = {
//    printCombatState(combatMap, 0)
    val result = simulateCombat(combatMap)
//    printCombatState(result._1, result._2)
    calculateScore(result)
  }

  def calculateScore(combatState: CombatState): Int = {
    val (combat, round, isIncompleteLast) = combatState
    if(isIncompleteLast) (round - 1) * winners(combat).map(_._2).map(_.hp).sum
    else round * winners(combat).map(_._2).map(_.hp).sum
  }

  def simulateCombat(combatMap: CombatMap): CombatState =
    LazyList.iterate((combatMap, 0, false))(simulateRound).find(isFinished).get

  def simulateRound(combat: CombatState): CombatState = {
    val (combatMap, round, _) = combat
    val (nextCombatMap, isIncompleteLast) = simulateRound(combatMap)
    //    printCombatState(combatMap, round)
    (nextCombatMap, round + 1, isIncompleteLast)
  }

  /**
    * We need to identify when a round finishes early by eliminating the last opponent when there are still characters with unfinished turns
    */
  def simulateRound(combat: CombatMap): (CombatMap, Boolean) = {
    val characters = combat.characters

    @tailrec
    def iterate(combatMap: CombatMap, remaining: Seq[(Coordinates, Character)]): (CombatMap, Boolean) = remaining match {
      case (coordinates, character) +: rest =>
        val (nextCombatMap, eliminated) = simulateTurn(combatMap)(coordinates, character)
        if(isFinished(nextCombatMap) && rest.nonEmpty) (nextCombatMap, true)
        else iterate(nextCombatMap, rest.filterNot { case (c, _) => eliminated.contains(c) })
      case _ => (combatMap, false)
    }

    iterate(combat, characters)
  }

  def simulateTurn(combatMap: CombatMap)(coordinates: Coordinates, character: Character): (CombatMap, Option[Coordinates]) = {
    if (canAttack(combatMap)(coordinates, character)) attack(combatMap)(coordinates, character)
    else if (canMove(combatMap)(coordinates, character)) {
      val (updated, moved) = move(combatMap)(coordinates, character)
      if (canAttack(updated)(moved, character)) attack(updated)(moved, character)
      else (updated, None)
    }
    else (combatMap, None)
  }

  def canAttack(combatMap: CombatMap)(coordinates: Coordinates, character: Character): Boolean  ={
    neighbours(combatMap)(coordinates).map(_._2).exists(isEnemy(character))
  }

  def attack(combatMap: CombatMap)(coordinates: Coordinates, character: Character): (CombatMap, Option[Coordinates]) = {
    val enemies = getEnemies(neighbours(combatMap)(coordinates))(character)
    val (c, toAttack) = enemies.minBy{ case ((x, y), enemy) => (enemy.hp, y, x) }
    val (attacked, survived) = attack(character, toAttack)

    if(survived) (CombatMap(combatMap.map.updated(c, attacked)), None)
    else (CombatMap(combatMap.map.updated(c, Empty)), Some(c))
  }

  def attack(attacker: Character, toAttack: Character): (Character, Boolean) = {
    val attacked = toAttack match {
      case Elf(a, hp)    => Elf(a, hp - attacker.attack)
      case Goblin(a, hp) => Goblin(a, hp - attacker.attack)
    }
    (attacked, attacked.hp > 0)
  }

  def getReachable(combatMap: CombatMap, from: Coordinates): Set[Coordinates] = {

    @tailrec
    def iterate(toTest: Seq[Coordinates], area: Set[Coordinates]): Set[Coordinates] = toTest match {
      case h +: rest =>
        val newTiles = neighbours(combatMap)(h).collect{ case (c, Empty) if !area.contains(c) => c}
        iterate(rest ++ newTiles, area ++ newTiles)
      case _ => area
    }

    iterate(Seq(from), Set())
  }

  def canMove(combatMap: CombatMap)(coordinates: Coordinates, character: Character): Boolean  = {
    val targets = getTargets(combatMap)(coordinates, character)
    if(targets.isEmpty) false
    else {
      val reachable = getReachable(combatMap, coordinates)
      targets.exists(reachable.contains)
    }
  }

  def getTargets(combatMap: CombatMap)(coordinates: Coordinates, character: Character): Seq[Coordinates] =
    getEnemies(combatMap.characters)(character).flatMap{ case (c, _) => neighbours(combatMap)(c)}.collect{ case (c, Empty) => c}

  def move(combatMap: CombatMap)(coordinates: Coordinates, character: Character): (CombatMap, Coordinates)  = {
    val reachableCells = getReachable(combatMap, coordinates)

    val reachable = getTargets(combatMap)(coordinates, character).filter(reachableCells.contains)
    val goal = findGoal(combatMap, coordinates)(reachable.toSet)

    val step = shortestPath(combatMap, coordinates)(goal).drop(1).head

    (CombatMap(combatMap.map.updated(coordinates, Empty).updated(step, character)), step)
  }

  /**
    * Notice how we pre collected all the reachable targets and iterate only until we found all the ones that are at most as far as the first found correct one
    */
  def findGoal(combatMap: CombatMap, from: Coordinates)(targets: Set[Coordinates]): Coordinates = {
    def streamSearch(initial: LazyList[(Coordinates, Int)], explored: Set[Coordinates]): LazyList[(Coordinates, Int)] = initial match {
      case (c, i) #:: rest if targets.contains(c) =>
        LazyList((c, i)) #::: streamSearch(rest, explored).takeWhile{case (_, i2) => i2 == i}
      case (c, i) #:: rest =>
        val more = emptyNewNeighbours(combatMap, explored)(c).map((_, i + 1))
        streamSearch(rest #::: more.to(LazyList), explored ++ more.map(_._1))
      case _ => LazyList()
    }
    streamSearch(LazyList((from, 0)), Set()).minBy { case ((x, y), d) => (d, y, x) }._1
  }

  /**
    * As long as we generate the next possible coordinates in reading order we will find the correct solution even when there are multiple shortest paths
    */
  def shortestPath(combatMap: CombatMap, from: Coordinates)(target: Coordinates): Seq[Coordinates] = {
        def streamSearch(initial: LazyList[List[Coordinates]], explored: Set[Coordinates]): LazyList[List[Coordinates]] = initial match {
          case (current :: path) #:: _ if current == target =>
            LazyList(current :: path)
          case (current :: path) #:: rest =>
            val more = emptyNewNeighbours(combatMap, explored)(current).map(_ :: current :: path)
            streamSearch(rest #::: more.to(LazyList), explored ++ more.flatten)
          case _ => LazyList()
        }

    streamSearch(LazyList(List(from)), Set()).find(_.head == target).map(_.reverse).get
  }

  def emptyNewNeighbours(combatMap: CombatMap, explored: Set[Coordinates])(coordinates: Coordinates): List[Coordinates] =
    neighbours(combatMap)(coordinates).collect{ case (c, Empty) if !explored.contains(c) => c}

  def neighbours(combatMap: CombatMap)(coordinates: Coordinates): List[(Coordinates, Tile)] =
    neighbours(coordinates).map(c => (c, combatMap.map(c)))

  /**
    * Returns the neighbours in reading order
    */
  def neighbours(coordinates: Coordinates): List[Coordinates] = {
    val (x, y) = coordinates
    List(
      (x, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x, y + 1),
    )
  }

  def isEnemy(character: Character)(tile: Tile): Boolean = (character, tile) match {
    case (Elf(_, _), Goblin(_, _)) => true
    case (Goblin(_, _), Elf(_, _)) => true
    case _ => false
  }

  def getEnemies(tiles: Seq[(Coordinates, Tile)])(character: Character): Seq[(Coordinates, Character)] =
    tiles.filter { case (_, tile) => isEnemy(character)(tile) }.map { case (c, char) => (c, char.asInstanceOf[Character]) }

  def isFinished(combat: CombatState): Boolean = isFinished(combat._1)

  def isFinished(combatMap: CombatMap): Boolean =
    combatMap.elves.isEmpty || combatMap.goblins.isEmpty

  def winners(combat: CombatMap): Seq[(Coordinates, Character)] =
    if(combat.elves.isEmpty) combat.goblins
    else combat.elves

  def asString(combatMap: CombatMap, withCharacters: Boolean = false): String = {
    val coordinates = combatMap.map.keys
    val xs = coordinates.map(_._1)
    val ys = coordinates.map(_._2)

    def char(tile: Tile): Char = tile match {
      case Empty => '.'
      case Wall => '#'
      case Elf(_, _) => 'E'
      case Goblin(_, _) => 'G'
    }

    def row(y: Int): String = {
      val row = (for (x <- xs.min to xs.max) yield char(combatMap.map((x, y)))).mkString

      if (withCharacters) {
        val characters = (for (x <- xs.min to xs.max) yield combatMap.map((x, y)))
          .collect {
            case Goblin(_, hp) => s"G($hp)"
            case Elf(_, hp) => s"E($hp)"
          }.mkString(", ")

        s"$row $characters"
      }
      else row
    }

    (for {y <- ys.min to ys.max} yield row(y)).mkString("\n")
  }

  def printCombatState(combatMap: CombatMap, round: Int, attack: Option[Int] = None): Unit = {
    attack.foreach(_ => println(s"Attack #$attack"))
    println(s"Round #$round")
    println(asString(combatMap, true))
  }

  override def part2(combatMap: CombatMap): Int = {
    printCombatState(combatMap, 0)

    val elvesWinWithoutLosses = LazyList.from(3).map(attack => {
      val result = simulateCombat(combatMap, attack)
      printCombatState(result._1, result._2, Some(attack))
      result
    }).find(doElvesWinWithoutLosses(combatMap.elves.size)).get

    calculateScore(elvesWinWithoutLosses)
  }

  def simulateCombat(combatMap: CombatMap, elvenAttack: Int): CombatState = {
    val modified = updateElvenAttack(combatMap, elvenAttack)
    simulateCombat(modified)
  }

  def updateElvenAttack(combat: CombatMap, attack: Int): CombatMap =
    combat.elves.foldLeft(combat) { case (c, (coordinates, elf)) => CombatMap(c.map.updated(coordinates, elf.copy(attack = attack))) }

  def doElvesWinWithoutLosses(elfSize: Int)(combatState: CombatState): Boolean = {
    val (combat, _, _) = combatState
    combat.elves.size == elfSize
  }
}
