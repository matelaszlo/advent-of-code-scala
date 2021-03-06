package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleMultiPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Sequences.{shiftLeft, shiftRight}
import com.lmat.util.Strings.leftPad

object Day10 extends SimpleMultiPuzzle[Seq[Int], Int, Seq[Int], String] {
  val size   = 256
  val rounds = 64

  override def parse1(resource: String): Seq[Int] = readResource(resource).head.split(",").map(_.toInt).toIndexedSeq

  override def parse2(resource: String): Seq[Int] = calculateLengths(readResource(resource).head)

  def calculateLengths(source:String): Seq[Int] =
    source.toIndexedSeq.map(_.toInt) ++ Seq(17, 31, 73, 47, 23)

  override def part1(lengths: Seq[Int]): Int =
    knotHashRound(initialState(size), lengths)
      .circularList.take(2).product

  case class State(circularList: Seq[Int], position: Int, skip: Int)

  def initialState(size: Int): State =
    State(0 until size, 0, 0)

  def knotHashRound(startState: State, lengths: Seq[Int]): State =
    lengths.foldLeft(startState)((state, length) => nextState(state, length))

  def nextState(state: State, length: Int): State = {
    val (toReverse, rest) = shiftLeft(state.circularList, state.position).splitAt(length)
    val reversed = toReverse.reverse
    val circularList = shiftRight(reversed ++ rest, state.position)

    val position = (state.position + length + state.skip) % state.circularList.size
    val skip = state.skip + 1
    State(circularList, position, skip)
  }

  override def part2(lengths: Seq[Int]): String =
    knotHash(size, rounds)(lengths)

  def knotHash(size: Int, rounds: Int)(lengths: Seq[Int]): String = {
    val sparse = sparseHash(initialState(size), lengths, rounds)
    val dense = denseHash(sparse)
    dense.map(num => leftPad(num.toHexString)('0', 2)).mkString
  }

  def sparseHash(startState: State, lengths: Seq[Int], rounds: Int): State =
    (0 until rounds).foldLeft(startState)((state, _) => knotHashRound(state, lengths))

  def denseHash(state: State, size: Int = 16): Seq[Int] =
    state.circularList.sliding(size, size).map(xor).toSeq

  def xor(seq: Seq[Int]): Int =
    seq.foldLeft(0)(_ ^ _)
}
