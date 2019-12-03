package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day02 extends SimpleCommonPuzzle[Vector[Int], Int, Int] {
  override def parse(resource: String): Vector[Int] =
    readResource(resource).flatMap(row => row.split(",").flatMap(raw => Try(raw.toInt).toOption)).toVector

  override def part1(intCode: Vector[Int]): Int = solveProgram(intCode)(12, 2)

  def solveProgram(intCode: Vector[Int])(noun: Int, verb: Int): Int =
    solve(intCode.updated(1, noun).updated(2, verb)).head

  def solve(intCode: Vector[Int]): Vector[Int] = {

    def update(code: Vector[Int], current: Int, operation: (Int, Int) => Int): Vector[Int] =
      code.updated(code(current + 3), operation(code(code(current + 1)), code(code(current + 2))))

    @tailrec
    def iterate(code: Vector[Int], current: Int): Vector[Int] = code(current) match {
      case 99 => code
      case 1  => iterate(update(code, current, _ + _), current + 4)
      case 2  => iterate(update(code, current, _ * _), current + 4)
      case _  => throw new IllegalArgumentException("")
    }

    iterate(intCode, 0)
  }

  override def part2(intCode: Vector[Int]): Int =
    generateInputs
//      .map { case (noun, verb) => println(s"Trying: $noun and $verb"); (noun, verb)} // If you are interested what generateInputs produce
      .find{ case (noun, verb) => trySolveProgram(intCode)(noun,verb).contains(19690720) }
      .map { case (noun, verb) => 100 * noun + verb }
      .get

  lazy val generateInputs: LazyList[(Int, Int)] =
    LazyList.from(0).flatMap(variations(_).to(LazyList))

  def variations(a: Int): Set[(Int, Int)] =
    (0 to a).flatMap(b => Seq((a, b), (b, a))).toSet

  def trySolveProgram(intCode: Vector[Int])(noun: Int, verb: Int): Option[Int] =
    Try(solveProgram(intCode)(noun, verb)).toOption
}
