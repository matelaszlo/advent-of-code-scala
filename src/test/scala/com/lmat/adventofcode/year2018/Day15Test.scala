package com.lmat.adventofcode.year2018

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import com.lmat.adventofcode.year2018.Day15Definitions._
import com.lmat.adventofcode.year2018.Day15._


class Day15Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val rawCombat: String =
    """#######
      |#.G.E.#
      |#E.G.E#
      |#.G.E.#
      |#######""".stripMargin

  val combat: CombatMap = CombatMap(Map(
    (0, 0) -> Wall, (1, 0) -> Wall,        (2, 0) -> Wall,           (3, 0) -> Wall,           (4, 0) -> Wall,        (5, 0) -> Wall,        (6, 0) -> Wall,
    (0, 1) -> Wall, (1, 1) -> Empty,       (2, 1) -> Goblin(3, 200), (3, 1) -> Empty,          (4, 1) -> Elf(3, 200), (5, 1) -> Empty,       (6, 1) -> Wall,
    (0, 2) -> Wall, (1, 2) -> Elf(3, 200), (2, 2) -> Empty,          (3, 2) -> Goblin(3, 200), (4, 2) -> Empty,       (5, 2) -> Elf(3, 200), (6, 2) -> Wall,
    (0, 3) -> Wall, (1, 3) -> Empty,       (2, 3) -> Goblin(3, 200), (3, 3) -> Empty,          (4, 3) -> Elf(3, 200), (5, 3) -> Empty,       (6, 3) -> Wall,
    (0, 4) -> Wall, (1, 4) -> Wall,        (2, 4) -> Wall,           (3, 4) -> Wall,           (4, 4) -> Wall,        (5, 4) -> Wall,        (6, 4) -> Wall
  ))

  test("Day15 - Parse") {
    assert(parseCombatState(rawCombat) == combat)
  }

  test("Day15 - Get Characters in Order") {
    assert(combat.characters == Seq(
      ((2, 1), Goblin(3, 200)),
      ((4, 1), Elf(3, 200)),
      ((1, 2), Elf(3, 200)),
      ((3, 2), Goblin(3, 200)),
      ((5, 2), Elf(3, 200)),
      ((2, 3), Goblin(3, 200)),
      ((4, 3), Elf(3, 200))))
  }

  test("Day15 - Attack Test") {
    val combatMap: CombatMap = parseCombatState(
      """G....
        |..G..
        |..EG.
        |..G..
        |...G.""".stripMargin
    )

    val (coordinates, elf) = ((2, 2), Elf(3, 200))

    val startMap = updateGoblinHealths(combatMap)(Seq(9, 4, 2, 2, 1))
    assert(canAttack(startMap)(coordinates, elf))
    val (map1, removed) = attack(startMap)(coordinates, elf)
    assert(map1.map(3, 2) == Empty)
    assert(removed.contains((3, 2)))

    assert(canAttack(map1)(coordinates, elf))
    val (map2, removed2) = attack(map1)(coordinates, elf)
    assert(map2.map(2, 3) == Empty)
    assert(removed2.contains((2, 3)))

    assert(canAttack(map2)(coordinates, elf))
    val (map3, removed3) = attack(map2)(coordinates, elf)
    assert(map3.map(2, 1) == Goblin(3, 1))
    assert(removed3.isEmpty)

    assert(canAttack(map3)(coordinates, elf))
    val (map4, removed4) = attack(map3)(coordinates, elf)
    assert(map4.map(2, 1) == Empty)
    assert(removed4.contains((2, 1)))

    assert(!canAttack(map4)(coordinates, elf))
  }

  def updateGoblinHealths(combatMap: CombatMap)(goblinHealths: Seq[Int]): CombatMap = {
    val reducedHealthGoblins = combatMap.goblins.sortBy { case ((x, y), _) => (y, x) }.zip(goblinHealths).map { case ((c, g), h) => (c, g.copy(hp = h)) }
    reducedHealthGoblins.foldLeft(combatMap) { case (map, (c, goblin)) => CombatMap(map.map.updated(c, goblin)) }
  }

  def updateElfHealths(combatMap: CombatMap)(elfHealths: Seq[Int]): CombatMap = {
    val reducedHealthElves = combatMap.elves.sortBy { case ((x, y), _) => (y, x) }.zip(elfHealths).map { case ((c, g), h) => (c, g.copy(hp = h)) }
    reducedHealthElves.foldLeft(combatMap) { case (map, (c, goblin)) => CombatMap(map.map.updated(c, goblin)) }
  }

  test("Day15 - Pathfinding") {
    val combatMap: CombatMap = parseCombatState(
      """#######
        |#E..G.#
        |#...#.#
        |#.G.#G#
        |#######""".stripMargin
    )

    val (coordinates, elf) = ((1, 1), Elf(3, 200))

    assert(getTargets(combatMap)(coordinates, elf).sortBy { case (x, y) => (y, x) } ==
      Seq((3, 1), (5, 1), (2, 2), (5, 2), (1, 3), (3, 3)))

    assert(getReachable(combatMap, coordinates).toSeq.sortBy { case (x, y) => (y, x) } ==
      Seq((2, 1), (3, 1), (1, 2), (2, 2), (3, 2), (1, 3), (3, 3)))

    assert(canMove(combatMap)(coordinates, elf))

    assert(shortestPath(combatMap, coordinates)(3, 1) == Seq((1, 1), (2, 1), (3, 1)))
  }

  test("Day15 - Move") {
    val combatMap: CombatMap = parseCombatState(
      """#######
        |#E..G.#
        |#...#.#
        |#.G.#G#
        |#######""".stripMargin)

    val (coordinates, elf) = ((1, 1), Elf(3, 200))

    assert(move(combatMap)(coordinates, elf)._1 == CombatMap(combatMap.map.updated(coordinates, Empty).updated((2, 1), elf)))
  }

  val combatMap0: CombatMap = parseCombatState(
    """#######
      |#.G...#
      |#...EG#
      |#.#.#G#
      |#..G#E#
      |#.....#
      |#######""".stripMargin)

  val combatMap0Solved: String =
    """#######
      |#G....#
      |#.G...#
      |#.#.#G#
      |#...#.#
      |#....G#
      |#######""".stripMargin

  val combatMap1: CombatMap = parseCombatState(
    """#######
      |#G..#E#
      |#E#E.E#
      |#G.##.#
      |#...#E#
      |#...E.#
      |#######""".stripMargin)

  val combatMap1Solved: String =
    """#######
      |#...#E#
      |#E#...#
      |#.E##.#
      |#E..#E#
      |#.....#
      |#######""".stripMargin

  val combatMap2: CombatMap = parseCombatState(
    """#######
      |#E..EG#
      |#.#G.E#
      |#E.##E#
      |#G..#.#
      |#..E#.#
      |#######""".stripMargin)

  val combatMap2Solved: String =
    """#######
      |#.E.E.#
      |#.#E..#
      |#E.##.#
      |#.E.#.#
      |#...#.#
      |#######""".stripMargin

  val combatMap3: CombatMap = parseCombatState(
    """#######
      |#E.G#.#
      |#.#G..#
      |#G.#.G#
      |#G..#.#
      |#...E.#
      |#######""".stripMargin)

  val combatMap3Solved: String =
    """#######
      |#G.G#.#
      |#.#G..#
      |#..#..#
      |#...#G#
      |#...G.#
      |#######""".stripMargin

  val combatMap4: CombatMap = parseCombatState(
    """#######
      |#.E...#
      |#.#..G#
      |#.###.#
      |#E#G#G#
      |#...#G#
      |#######""".stripMargin)

  val combatMap4Solved: String =
    """#######
      |#.....#
      |#.#G..#
      |#.###.#
      |#.#.#.#
      |#G.G#G#
      |#######""".stripMargin

  val combatMap5: CombatMap = parseCombatState(
    """#########
      |#G......#
      |#.E.#...#
      |#..##..G#
      |#...##..#
      |#...#...#
      |#.G...G.#
      |#.....G.#
      |#########""".stripMargin)

  val combatMap5Solved: String =
    """#########
      |#.G.....#
      |#G.G#...#
      |#.G##...#
      |#...##..#
      |#.G.#...#
      |#.......#
      |#.......#
      |#########""".stripMargin

  val combatScenarios =
    Table(
      ("combat",   "solution",       "characters",                                                                                                                          "part1", "part2"),
      (combatMap1, combatMap1Solved, Seq(((5, 1), Elf(3, 200)),    ((1, 2), Elf(3, 197)),    ((2, 3), Elf(3, 185)),    ((1, 4), Elf(3, 200)),    ((5, 4), Elf(3, 200))),    36334,   29064),
      (combatMap2, combatMap2Solved, Seq(((2, 1), Elf(3, 164)),    ((4, 1), Elf(3, 197)),    ((3, 2), Elf(3, 200)),    ((1, 3), Elf(3, 98)),     ((2, 4), Elf(3, 200))),    39514,   31284),
      (combatMap3, combatMap3Solved, Seq(((1, 1), Goblin(3, 200)), ((3, 1), Goblin(3, 98)),  ((3, 2), Goblin(3, 200)), ((5, 4), Goblin(3, 95)),  ((4, 5), Goblin(3, 200))), 27755,   3478),
      (combatMap4, combatMap4Solved, Seq(((3, 2), Goblin(3, 200)), ((1, 5), Goblin(3, 98)),  ((3, 5), Goblin(3, 38)),  ((5, 5), Goblin(3, 200))),                           28944,   6474),
      (combatMap5, combatMap5Solved, Seq(((2, 1), Goblin(3, 137)), ((1, 2), Goblin(3, 200)), ((3, 2), Goblin(3, 200)), ((2, 3), Goblin(3, 200)), ((2, 5), Goblin(3, 200))), 18740,   1140)
    )

  test("Day15 - Part 1") {
    forAll(combatScenarios) { (combat, solution, characterList, part1Solution, _) =>
      val (solved, _, _) = simulateCombat(combat)

      assert(asString(solved)  == solution)
      assert(solved.characters == characterList)
      assert(part1(combat)     == part1Solution)
    }
  }

  test("Day15 - Part 2") {
    forAll(combatScenarios) { (combat, _, _, _, part2Solution) =>
      assert(part2(combat) == part2Solution)
    }
  }

  test("Day15 - Hard Path") {
    val combatMap = parseCombatState(
      """################################
        |#########....#..#####.......####
        |###########.......###..##..#####
        |###########.....#.###......#####
        |###############.#...#.......####
        |###############..#...GE.G....###
        |############.##...#...GEG..#####
        |############.##........G...#####
        |###########..##.......G..G######
        |#..####.##...##....#...GGE######
        |#..........#............#.######
        |#.......#..............##..#...#
        |#.............#####...####...#.#
        |#........#...#######.G#####....#
        |#.##........#########.#######E.#
        |#...........#########.#######.##
        |####........#########.##########
        |##.#........#########.##########
        |##..........#########.##########
        |##...........#######..##########
        |#....#.....G..#####...##########
        |#......#...GG...GE....##########
        |###.#.........GGE....###########
        |###..........GEE....E###.#######
        |####........G.GE......#....#####
        |####.####....G..####......######
        |####..#####.....####......######
        |#############..#####......######
        |#####################.....######
        |#####################..#..######
        |#####################.##########
        |################################""".stripMargin)

    val (coordinates, _) = ((22, 8), Goblin(3, 200))

    assert(getReachable(combatMap, coordinates).size == 298)
    assert(findGoal(combatMap, coordinates)(Set((22, 4), (15, 25))) == (22, 4))
    assert(shortestPath(combatMap, coordinates)((22, 4)) == Seq((22,8), (23,8), (24,8), (24,7), (25,7), (25,6), (25,5), (25,4), (24,4), (23,4), (22,4)))
  }

  test("Day15 - Hard Path 2") {
    val combatMap = parseCombatState(
      """################################
        |#########....#..#####.......####
        |###########.......###..##..#####
        |###########.....#.###......#####
        |###############.#...#.......####
        |###############..#...GEG.....###
        |############.##...#...GEG..#####
        |############.##........G...#####
        |###########..##........G.G######
        |#..####.##...##....#...GGE######
        |#..........#............#.######
        |#.......#..............##..#...#
        |#.............#####...####...#.#
        |#........#...#######.G#####....#
        |#.##........#########.#######E.#
        |#...........#########.#######.##
        |####........#########.##########
        |##.#........#########.##########
        |##..........#########.##########
        |##...........#######..##########
        |#....#.....G..#####...##########
        |#......#...GG...GE....##########
        |###.#.........GGE....###########
        |###..........GEE....E###.#######
        |####........G.GE......#....#####
        |####.####....G..####......######
        |####..#####.....####......######
        |#############..#####......######
        |#####################.....######
        |#####################..#..######
        |#####################.##########
        |################################""".stripMargin)

    val (coordinates, _) = ((23, 9), Goblin(3, 200))

    assert(getReachable(combatMap, coordinates).size == 263)
    assert(findGoal(combatMap, coordinates)(Set((15, 25))) == (15, 25))
    assert(shortestPath(combatMap, coordinates)((15, 25)) == Seq((23,9), (22,9), (21,9), (20,9), (20,10), (19,10), (18,10), (17,10), (16,10), (15,10), (14,10), (13,10), (12,10), (12,11), (11,11), (10,11), (10,12), (10,13), (10,14), (10,15), (10,16), (10,17), (10,18), (10,19), (10,20), (10,21), (10,22), (11,22), (11,23), (11,24), (11,25), (12,25), (12,26), (13,26), (14,26), (14,25), (15,25)))
  }
}
