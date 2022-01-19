package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2021.Day08Definitions._
import com.lmat.util.Files.readResource

object Day08Definitions {
  case class ScrambledDisplay(inputs: List[Set[Char]], outputs: List[Set[Char]])
}

object Day08 extends SimpleCommonPuzzle[List[ScrambledDisplay], Int, Int] {
  override def parse(resource: String): List[ScrambledDisplay] =
    readResource(resource).flatMap(parseLine).toList

  def parseLine(line: String): Option[ScrambledDisplay] = {
    val parsed: List[List[Set[Char]]] = line.split(" \\| ").map(_.split(" ").map(_.toCharArray.toSet).toList).toList
    for {
      inputs <- parsed.headOption
      outputs <- parsed.lift(1)
    } yield ScrambledDisplay(inputs, outputs)
  }

  override def part1(displays: List[ScrambledDisplay]): Int =
    displays.flatMap(_.outputs).count(n => Set(2, 3, 4, 7).contains(n.size))

  override def part2(displays: List[ScrambledDisplay]): Int =
    displays.map(output).sum

  // By looking at the segments we can come up with some deduction rules that help differentiate the numbers
  def deduction(inputs: List[Set[Char]]): Map[Set[Char], Int] = {
    val one          = inputs.find(_.size == 2).get
    val seven        = inputs.find(_.size == 3).get
    val four         = inputs.find(_.size == 4).get
    val eight        = inputs.find(_.size == 7).get
    val twoThreeFive = inputs.filter(_.size == 5)
    val zeroSixNine  = inputs.filter(_.size == 6)

    val a  = seven -- one
    val eg = eight -- four -- a
    val bd = eight -- seven -- eg

    val (two, threeFive) = twoThreeFive.partition(eg.subsetOf)
    val (five, three)    = threeFive.partition(bd.subsetOf)

    val (zeroSix, nine) = zeroSixNine.partition(eg.subsetOf)
    val (six, zero)     = zeroSix.partition(bd.subsetOf)
   Map(
     zero.head  -> 0,
     one        -> 1,
     two.head   -> 2,
     three.head -> 3,
     four       -> 4,
     five.head  -> 5,
     six.head   -> 6,
     seven      -> 7,
     eight      -> 8,
     nine.head  -> 9,
   )
  }

  def output(display: ScrambledDisplay): Int = {
    val table = deduction(display.inputs)
    display.outputs.map(table).mkString("").toInt
  }
}
