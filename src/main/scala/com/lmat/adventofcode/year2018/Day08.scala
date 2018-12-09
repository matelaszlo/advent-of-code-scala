package com.lmat.adventofcode.year2018

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.adventofcode.year2018.Day08Definitions._
import com.lmat.util.Files.readResource

object Day08Definitions {
  case class Node(children: Seq[Node], metaData: Seq[Int])
}

object Day08 extends CommonPuzzle[Seq[Int], Node, Int, Int] {
  override def parse(resource: String): Seq[Int] =
    readResource(resource).head.split(" ").toSeq.map(_.toInt)

  def buildTree(numbers: Seq[Int]): Node =  {
    def buildNodes(count: Int, numbers: Seq[Int]): (Seq[Node], Seq[Int]) =
      if(count == 0) (Seq[Node](), numbers)
      else {
        val (n1, remaining) = buildNode(numbers)
        val (ns, rest) = buildNodes(count - 1, remaining)
        (n1 +: ns, rest)
      }

    def buildNode(numbers: Seq[Int]): (Node, Seq[Int]) = {
      val childCount +: metaDataCount +: current  = numbers
      val (children, remaining) = buildNodes(childCount, current)
      val (metaData, rest) = remaining.splitAt(metaDataCount)
      (Node(children, metaData), rest)
    }

    buildNode(numbers)._1
  }

  override def preProcess(numbers: Seq[Int]): Node = buildTree(numbers)

  override def part1(tree: Node): Int = metaDataSum(tree)

  def metaDataSum(node: Node): Int =
    node.metaData.sum + node.children.map(metaDataSum).sum

  override def part2(tree: Node): Int = nodeValue(tree)

  def nodeValue(node: Node): Int =
    if (node.children.isEmpty) node.metaData.sum
    else node.metaData.map(m => node.children.lift(m - 1).map(nodeValue).getOrElse(0)).sum
}
