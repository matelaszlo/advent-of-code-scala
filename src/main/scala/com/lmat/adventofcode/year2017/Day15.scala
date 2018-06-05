package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.Puzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Strings.leftPad

import scala.annotation.tailrec

object Day15 extends Puzzle[(Long, Long), Long, Long] {
  override def parse(resource: String): (Long, Long) = {
    val starts = readResource(resource).map(_.split(" ").last)
    (starts.head.toLong, starts(1).toLong)
  }

  override def part1(starts: (Long, Long)): Long = {
    val (startA, startB) = starts
    val generatorA = generate(16807L, 2147483647L)(_)
    val generatorB = generate(48271L, 2147483647L)(_)
    val n = 40000000
    judge(generatorA, generatorB, startA, startB, n)
  }

  override def part2(starts: (Long, Long)): Long = {
    val (startA, startB) = starts
    val generatorA = generateWithTest(16807L, 2147483647L, 4)(_)
    val generatorB = generateWithTest(48271L, 2147483647L, 8)(_)
    val n = 5000000
    judge(generatorA, generatorB, startA, startB, n)
  }

  def judge(genA: Long => Long, genB: Long => Long, startA: Long, startB: Long, n: Long): Long = {

    @tailrec
    def count(a: Long, b: Long, n: Long, c: Long): Long =
      if (n == 0) c
      else {
        val newA = genA(a)
        val newB = genB(b)
        val newC = if (lowestNBits(newA, 16) == lowestNBits(newB, 16)) c + 1 else c
        count(newA, newB, n - 1, newC)
      }

    count(startA, startB, n, 0)
  }

  def lowestNBits(value: Long, n: Int): String = {
    val binary = value.toBinaryString
    leftPad(binary.drop(binary.length - n))('0', n)
  }

  def generate(factor: Long, mod: Long)(value: Long): Long =
    value * factor % mod

  @tailrec
  def generateWithTest(factor: Long, mod: Long, test: Long)(value: Long): Long = {
    val newValue = generate(factor, mod)(value)
    if (newValue % test == 0) newValue
    else generateWithTest(factor, mod, test)(newValue)
  }
}
