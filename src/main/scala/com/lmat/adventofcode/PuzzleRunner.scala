package com.lmat.adventofcode

object PuzzleRunner extends App {
  val year = args.lift(0).map(_.toInt).getOrElse(2017)
  val day  = args.lift(1).map(_.toInt).getOrElse(1)

  def puzzleMap = Map(
    (2017, 1)  -> year2017.Day01,
    (2017, 2)  -> year2017.Day02,
    (2017, 3)  -> year2017.Day03,
    (2017, 4)  -> year2017.Day04,
    (2017, 5)  -> year2017.Day05,
    (2017, 6)  -> year2017.Day06,
    (2017, 7)  -> year2017.Day07,
    (2017, 8)  -> year2017.Day08,
    (2017, 9)  -> year2017.Day09,
    (2017, 10) -> year2017.Day10
  )

  run(puzzleMap, year, day)

  def resource(year: Int, day: Int): String = s"$year/Day${"%02d".format(day)}.txt"

  def run(puzzleMap: Map[(Int, Int), Puzzle[_, _, _]], year: Int, day: Int): Unit =
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
