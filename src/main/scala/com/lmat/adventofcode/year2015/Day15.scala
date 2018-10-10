package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.adventofcode.year2015.Day15Definitions._
import com.lmat.util.Files.readResource

object Day15Definitions {
  case class Ingredient(name: String, capacity: Int, durability: Int, flavour: Int, texture: Int, calories: Int)
}

object Day15 extends CommonPuzzle[Seq[Ingredient], Seq[Map[Ingredient, Int]], Int, Int]{
  override def parse(resource: String): Seq[Ingredient] = readResource(resource).flatMap(parseIngredient)

  def parseIngredient(row: String): Option[Ingredient] = {
    val ingredient = "(.*): capacity (.*), durability (.*), flavor (.*), texture (.*), calories (.*)".r

    row match {
      case ingredient(name, capacity, durability, flavour, texture, calories) =>
        Some(Ingredient(name, capacity.toInt, durability.toInt, flavour.toInt, texture.toInt, calories.toInt))
      case _ =>
        None
    }
  }

  override def preProcess(ingredients: Seq[Ingredient]): Seq[Map[Ingredient, Int]] =
    combinations(ingredients, 100)

  /**
    * We can do combinations with repetitions by repeating our input first
    * Then invoking the combinations function on collections
    */
  def combinations(ingredients: Seq[Ingredient], n:Int): Seq[Map[Ingredient, Int]] = {
    ingredients
      .flatMap(i => List.fill(n)(i.name))
      .combinations(n)
      .toStream
      .map(_.groupBy(n => ingredients.find(_.name == n).get).mapValues(_.size))
  }

  override def part1(combinations: Seq[Map[Ingredient, Int]]): Int =
    combinations.map(score).max

  def score(ingredients: Map[Ingredient, Int]): Int =
    ingredients
      .map { case (i, n) => score(i, n) }
      .fold(Seq())((total, current) =>
        if (total.isEmpty) current
        else (total zip current).map { case (a, b) => a + b })
      .map(c => if (c < 0) 0 else c)
      .product

  def score(ingredient: Ingredient, n: Int): Seq[Int] =
    Seq(ingredient.capacity * n, ingredient.durability * n, ingredient.flavour * n, ingredient.texture * n)

  override def part2(combinations: Seq[Map[Ingredient, Int]]): Int =
    combinations.filter(calories(_) == 500).map(score).max

  def calories(ingredients: Map[Ingredient, Int]): Int =
    ingredients.map{case (i, n) => i.calories * n}.sum

}
