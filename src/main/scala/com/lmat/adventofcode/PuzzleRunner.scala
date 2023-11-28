package com.lmat.adventofcode

import scala.util.Try

object PuzzleRunner extends App {
  val year = args.headOption.map(_.toInt).getOrElse(2020)
  val day = args.lift(1).map(_.toInt).getOrElse(1)
  lazy val puzzleMap = buildPuzzleMap(2015, 2023)
  run(puzzleMap, year, day)

  def buildPuzzleMap(from: Int, to: Int): Map[(Int, Int), Puzzle[_, _, _, _, _, _]] = {
    for {
      year   <- from to to
      day    <- 1 to 25
      puzzle <- puzzleReference(year, day)
    } yield ((year, day), puzzle)
  }.toMap

  // Maybe this should be a macro instead of reflection
  def puzzleReference(year: Int, day: Int): Option[Puzzle[_, _, _, _, _, _]] = Try {
    import scala.reflect.runtime.{universe => ru}
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val path = s"com.lmat.adventofcode.year$year.Day${"%02d".format(day)}"
    val staticModule = runtimeMirror.staticModule(path)
    val reflectModule = runtimeMirror.reflectModule(staticModule)
    reflectModule.instance.asInstanceOf[Puzzle[_, _, _, _, _, _]]
  }.toOption

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
