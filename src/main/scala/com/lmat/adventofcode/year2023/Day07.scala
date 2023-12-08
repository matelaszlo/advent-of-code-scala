package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2023.Day07Definitions._
import com.lmat.util.Files.readResource

object Day07Definitions {
  case class Hand(cards: List[Char], bid: Int)
}

object Day07 extends SimpleCommonPuzzle[List[Hand], Long, Long] {
  override def parse(resource: String): List[Hand] =
    readResource(resource).toList.map(parseHand)

  def parseHand(row: String): Hand = {
    val hand :: bid :: Nil = row.split(" ").toList
    Hand(hand.toCharArray.toList, bid.toInt)
  }

  override def part1(hands: List[Hand]): Long = {
    implicit val ordering: Ordering[Hand] = order(evaluate, List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2'))
    totalWinnings(hands)
  }

  def evaluate(cards: List[Char]): String =
    cards.groupBy(identity).map { case (k, v) => (k, v.size) }.toList.map(_._2).sorted.reverse match {
      case 5 :: Nil           => "Five of a kind"
      case 4 :: 1 :: Nil      => "Four of a kind"
      case 3 :: 2 :: Nil      => "Full house"
      case 3 :: _             => "Three of a kind"
      case 2 :: 2 :: 1 :: Nil => "Two pairs"
      case 2 :: _             => "One pair"
      case _                  => "High card"
    }

  override def part2(hands: List[Hand]): Long = {
    implicit val ordering: Ordering[Hand] = order(evaluate2, List('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J'))
    totalWinnings(hands)
  }

  // We generate all sensible joker replacements and choose the best one
  def evaluate2(cards: List[Char]): String =
    replaceAll(cards).map(evaluate).min

  def replaceAll(cards: List[Char]): List[List[Char]] = {
    val (joker, normal)  = cards.partition(_ =='J')
    if(joker.isEmpty) List(cards)
    else if(joker.size == 5) List(List('A', 'A', 'A', 'A', 'A'))
    else {
      val possible = normal.toSet
      val count = joker.size

      possible.toList.flatMap(p => List.fill(count)(p)).combinations(count).toList.map(replacements => normal ++ replacements)
    }
  }

  def totalWinnings(hands: List[Hand])(implicit order: Ordering[Hand]): Long =
    hands.sorted.reverse.zipWithIndex.map { case (hand, index) => hand.bid.toLong * (index + 1).toLong }.sum

  def order(evaluate: List[Char] => String, cards: List[Char]): Ordering[Hand] =
    Ordering.by[Hand, (Int, Int, Int, Int, Int, Int)](hand =>
      (
        List("Five of a kind", "Four of a kind", "Full house", "Three of a kind", "Two pairs", "One pair", "High card").indexOf(evaluate(hand.cards)),
        cards.indexOf(hand.cards.head),
        cards.indexOf(hand.cards(1)),
        cards.indexOf(hand.cards(2)),
        cards.indexOf(hand.cards(3)),
        cards.indexOf(hand.cards(4))
      ))
}
