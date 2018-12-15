package com.lmat.adventofcode

object PuzzleRunner extends App {
  val year = args.headOption.map(_.toInt).getOrElse(2017)
  val day  = args.lift(1).map(_.toInt).getOrElse(1)

  def puzzleMap = Map(
    // 2015
    (2015, 1)  -> year2015.Day01,
    (2015, 2)  -> year2015.Day02,
    (2015, 3)  -> year2015.Day03,
    (2015, 4)  -> year2015.Day04,
    (2015, 5)  -> year2015.Day05,
    (2015, 6)  -> year2015.Day06,
    (2015, 7)  -> year2015.Day07,
    (2015, 8)  -> year2015.Day08,
    (2015, 9)  -> year2015.Day09,
    (2015, 10) -> year2015.Day10,
    (2015, 11) -> year2015.Day11,
    (2015, 12) -> year2015.Day12,
    (2015, 13) -> year2015.Day13,
    (2015, 14) -> year2015.Day14,
    (2015, 15) -> year2015.Day15,
    (2015, 16) -> year2015.Day16,
    (2015, 17) -> year2015.Day17,
    (2015, 18) -> year2015.Day18,
    (2015, 19) -> year2015.Day19,
    (2015, 20) -> year2015.Day20,
    (2015, 21) -> year2015.Day21,
    (2015, 22) -> year2015.Day22,
    (2015, 23) -> year2015.Day23,
    (2015, 24) -> year2015.Day24,
    (2015, 25) -> year2015.Day25,

    // 2017
    (2017, 1)  -> year2017.Day01,
    (2017, 2)  -> year2017.Day02,
    (2017, 3)  -> year2017.Day03,
    (2017, 4)  -> year2017.Day04,
    (2017, 5)  -> year2017.Day05,
    (2017, 6)  -> year2017.Day06,
    (2017, 7)  -> year2017.Day07,
    (2017, 8)  -> year2017.Day08,
    (2017, 9)  -> year2017.Day09,
    (2017, 10) -> year2017.Day10,
    (2017, 11) -> year2017.Day11,
    (2017, 12) -> year2017.Day12,
    (2017, 13) -> year2017.Day13,
    (2017, 14) -> year2017.Day14,
    (2017, 15) -> year2017.Day15,
    (2017, 16) -> year2017.Day16,
    (2017, 17) -> year2017.Day17,
    (2017, 18) -> year2017.Day18,
    (2017, 19) -> year2017.Day19,
    (2017, 20) -> year2017.Day20,
    (2017, 21) -> year2017.Day21,
    (2017, 22) -> year2017.Day22,
    (2017, 23) -> year2017.Day23,
    (2017, 24) -> year2017.Day24,
    (2017, 25) -> year2017.Day25,

    // 2018
    (2018, 1)  -> year2018.Day01,
    (2018, 2)  -> year2018.Day02,
    (2018, 3)  -> year2018.Day03,
    (2018, 4)  -> year2018.Day04,
    (2018, 5)  -> year2018.Day05,
    (2018, 6)  -> year2018.Day06,
    (2018, 7)  -> year2018.Day07,
    (2018, 8)  -> year2018.Day08,
    (2018, 9)  -> year2018.Day09,
    (2018, 10) -> year2018.Day10,
    (2018, 11) -> year2018.Day11,
    (2018, 12) -> year2018.Day12,
    (2018, 13) -> year2018.Day13
  )

  run(puzzleMap, year, day)

  def resource(year: Int, day: Int): String = s"$year/Day${"%02d".format(day)}.txt"

  def run(puzzleMap: Map[(Int, Int), Puzzle[_, _, _, _, _, _]], year: Int, day: Int): Unit =
    puzzleMap.get(year, day) match {
      case None => println(s"Puzzle for Day $day (Year $year) is not yet solved!")
      case Some(puzzle) =>
        println(s"Solving puzzle for Day $day (Year $year)")

        val res = resource(year, day)
        println(s"Loading input from $res")

        val (result1, result2) = puzzle.solve(res)
        println(s"Part 1: $result1")
        println(s"Part 2: $result2")
    }
}
