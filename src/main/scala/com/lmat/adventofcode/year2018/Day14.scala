package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day14 extends SimpleCommonPuzzle[Int, String, Int] {
  override def parse(resource: String): Int = readResource(resource).headOption.flatMap(row => Try(row.toInt).toOption).get

  /**
    * If we can generate an infinite Stream of the recipes both parts become trivial
    */
  def recipeStream: Stream[Int] = {
    type State = (Vector[Int], Int, Int)

    def next(state: State): State = {
      val (recipes, i1, i2) = state
      val recipe1 = recipes(i1)
      val recipe2 = recipes(i2)

      val extendedRecipes = recipes ++ toDigits(recipe1 + recipe2)
      (extendedRecipes, shiftRight(i1, recipe1 + 1, extendedRecipes.length), shiftRight(i2, recipe2 + 1, extendedRecipes.length))
    }

    Stream.iterate((Vector(3, 7), 0, 1))(next).zipWithIndex.map { case ((recipes, _, _), index) => recipes(index) }
  }

  def toDigits(number: Int): Vector[Int] =
    number.toString.map(_.asDigit).toVector

  def shiftRight(i: Int, n: Int, length: Int): Int = {
    val shifted = i + n
    if(shifted < length) shifted else shifted % length
  }

  override def part1(number: Int): String =
    recipeStream.slice(number, number + 10).mkString

  override def part2(number: Int): Int =
    recipeStream.indexOfSlice(toDigits(number))
}
