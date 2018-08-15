package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day02Definitions._
import com.lmat.util.Files.readResource

object Day02Definitions {
  case class Box(length: Int, width:Int, height:Int)
}

object Day02 extends SimpleCommonPuzzle[Seq[Box], Int, Int] {
  override def parse(resource: String): Seq[Box] = readResource(resource).flatMap(parseBox)

  def parseBox(line:String): Option[Box] = {
    val box = "(.*)x(.*)x(.*)".r
    line match {
      case box(length, width, height) => Some(Box(length.toInt, width.toInt, height.toInt))
      case _                          => None
    }
  }

  override def part1(boxes: Seq[Box]): Int =
    boxes.map(wrapping).sum

  def wrapping(box: Box): Int =
    area(box) + sideAreas(box).min

  def area(box: Box): Int =
    2 * box.length * box.width +
      2 * box.width * box.height +
      2 * box.height * box.length

  def sideAreas(box: Box): Seq[Int] =
    (box.length * box.width) :: (box.width * box.height) :: (box.height * box.length) :: Nil

  override def part2(boxes: Seq[Box]): Int =
    boxes.map(ribbon).sum

  def ribbon(box: Box): Int =
    volume(box) + perimeters(box).min

  def volume(box: Box): Int =
    box.length * box.width * box.height

  def perimeters(box: Box): Seq[Int] =
    (box.length + box.width) * 2 :: (box.width + box.height) * 2 :: (box.height + box.length) * 2 :: Nil

}
