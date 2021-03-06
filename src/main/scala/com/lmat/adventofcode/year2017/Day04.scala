package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day04 extends SimpleCommonPuzzle[Seq[Seq[String]], Int, Int]{
  type PassPhrase = Seq[String]

  override def parse(resource: String): Seq[PassPhrase] =
    readResource(resource).map(_.split("\\s+").toSeq)

  override def part1(passPhrases: Seq[PassPhrase]): Int =
    countValidPassphrases(passPhrases, hasOnlyUniqueWords)

  override def part2(passPhrases: Seq[PassPhrase]): Int =
    countValidPassphrases(passPhrases, hasNoAnagrams)

  def countValidPassphrases(passPhrases:Seq[PassPhrase], criteria: PassPhrase => Boolean): Int =
    passPhrases.count(criteria)

  def hasOnlyUniqueWords(words: PassPhrase): Boolean =
    words.size == words.distinct.size

  def hasNoAnagrams(words: PassPhrase): Boolean =
    words.size == words.map(_.toSeq.sorted.unwrap).distinct.size
}
