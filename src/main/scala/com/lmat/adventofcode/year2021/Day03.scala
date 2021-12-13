package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day03 extends SimpleCommonPuzzle[List[String], Int, Int] {
  override def parse(resource: String): List[String] =
    readResource(resource).toList

  override def part1(diagnostics: List[String]): Int =
    powerConsumption(diagnostics)

  def powerConsumption(diagnostics: List[String]): Int =
    fromBinary(gammaRate(diagnostics)) * fromBinary(epsilonRate(diagnostics))

  def simpleRate(f: List[String] => Int => Char)(diagnostics: List[String]): String =
    diagnostics.head.indices.map(f(diagnostics)).mkString

  def gammaRate: List[String] => String =
    simpleRate(mostCommonBit)(_)

  def epsilonRate: List[String] => String =
    simpleRate(leastCommonBit)(_)

  override def part2(diagnostics: List[String]): Int =
    lifeSupportRating(diagnostics)

  def lifeSupportRating(diagnostics: List[String]): Int =
    fromBinary(oxygenGeneratorRating(diagnostics)) * fromBinary(co2ScrubberRating(diagnostics))

  def criteriaRate(f: List[String] => Char => Int => Char)(preferred: Char)(diagnostics: List[String]): String = {
    val maxSize = diagnostics.head.length
    @tailrec
    def loop(remaining: List[String], i: Int): String = {
//      println(s"I:$i Remaining:$remaining")
      if (i > maxSize) "NOT_FOUND"
      else if (remaining.size == 1) remaining.head
      else {
        val criteria = f(remaining)(preferred)(i)
        val filtered = remaining.filter(d => d(i) == criteria)
        loop(filtered, i + 1)
      }
    }
    loop(diagnostics, 0)
  }

  def oxygenGeneratorRating: List[String] => String =
    criteriaRate(mostCommonBitWithPreference)('1')(_)

  def co2ScrubberRating: List[String] => String =
    criteriaRate(leastCommonBitWithPreference)('0')(_)

  // Invariants for the helper functions below
  // We have a non empty list of binary numbers
  // They are all left padded with zeroes to the same length
  def mostCommonBit(binaries: List[String])(index: Int): Char =
    mostFrequent(binaries.map(_ (index)))

  def leastCommonBit(binaries: List[String])(index: Int): Char =
    invert(mostCommonBit(binaries)(index))

  def mostFrequent[A](seq: Seq[A]): A =
    seq.groupBy(a => a).view.mapValues(_.size)
      .maxBy { case (_, count) => count }
      ._1

  def mostCommonBitWithPreference(binaries: List[String])(preferred: Char)(index: Int): Char =
    mostFrequentWithPreference(binaries.map(_ (index)), preferred)

  // Note that we need to invert both preference and the output here
  def leastCommonBitWithPreference(binaries: List[String])(preferred: Char)(index: Int): Char =
    invert(mostCommonBitWithPreference(binaries)(invert(preferred))(index))

  def mostFrequentWithPreference[A](seq: Seq[A], preferred: A): A = {
    val occurrenceMap = seq.groupBy(a => a).view.mapValues(_.size)
    val maxSize = occurrenceMap.values.max
    val maxEntries = occurrenceMap.filter{ case (_, v) => v == maxSize}.keys.toSeq
    if(maxEntries.contains(preferred)) preferred else maxEntries.head
  }

  def fromBinary(binary: String): Int =
    Integer.parseInt(binary, 2)

  def invert(binaryChar: Char): Char = binaryChar match {
    case '0' => '1'
    case '1' => '0'
  }
}
