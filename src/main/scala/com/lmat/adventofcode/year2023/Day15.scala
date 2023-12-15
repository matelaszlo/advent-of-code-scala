package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleMultiPuzzle
import com.lmat.adventofcode.year2023.Day15Definitions._
import com.lmat.util.Files.readResource

object Day15Definitions {
  sealed trait Operation

  case class Equal(label: String, lens: Int) extends Operation

  case class Dash(label: String) extends Operation
}

object Day15 extends SimpleMultiPuzzle[List[String], Int, List[Operation], Int] {
  override def parse1(resource: String): List[String] =
    readResource(resource).toList.flatMap(_.split(","))

  override def parse2(resource: String): List[Operation] =
    readResource(resource).toList.flatMap(_.split(",")).flatMap(parseOperation)

  def parseOperation(raw: String): Option[Operation] = {
    val pattern1 = s"(.*)=(\\d+)".r
    val pattern2 = s"(.*)-".r

    raw match {
      case pattern1(label, lensRaw) => lensRaw.toIntOption.map(lens => Equal(label, lens))
      case pattern2(label) => Some(Dash(label))
      case _ => None
    }
  }

  override def part1(init: List[String]): Int =
    init.map(hash).sum

  def hash(raw: String): Int =
    raw.toCharArray.foldLeft(0)((current, c) => (current + c.toInt) * 17 % 256)


  override def part2(operations: List[Operation]): Int =
    focusingPower(simulate(operations))

  def simulate(operations: List[Operation]): Map[Int, Vector[(String, Int)]] =
    operations.foldLeft(Map.empty[Int, Vector[(String, Int)]])((state, op) => op match {
      case Equal(label, lens) => add(state)(hash(label), label, lens)
      case Dash(label) => remove(state)(hash(label), label)
    })

  def remove(state: Map[Int, Vector[(String, Int)]])(box: Int, label: String): Map[Int, Vector[(String, Int)]] =
    state.updated(box, state.getOrElse(box, Vector.empty).filterNot { case (l, _) => l == label })

  def add(state: Map[Int, Vector[(String, Int)]])(box: Int, label: String, lens: Int): Map[Int, Vector[(String, Int)]] =
    state.updated(box, {
      val lenses = state.getOrElse(box, Vector.empty)
      val added: Option[Vector[(String, Int)]] = lenses.zipWithIndex.find { case ((l, _), _) => l == label }.map { case (_, i) => lenses.updated(i, (label, lens)) }
      added.getOrElse(lenses.appended((label, lens)))
    })

  def focusingPower(state: Map[Int, Vector[(String, Int)]]): Int =
    state.toList.flatMap { case (box, lenses) => lenses.zipWithIndex.map { case ((_, l), i) => (box + 1) * (i + 1) * l } }.sum
}
