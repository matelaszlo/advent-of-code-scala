package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day07Definitions._
import com.lmat.adventofcode.year2023.Day07._
class Day07Test extends AnyFunSuite {
  val raw =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin

  val parsed = List(
    Hand(List('3', '2', 'T', '3', 'K'),765),
    Hand(List('T', '5', '5', 'J', '5'),684),
    Hand(List('K', 'K', '6', '7', '7'),28),
    Hand(List('K', 'T', 'J', 'J', 'T'),220),
    Hand(List('Q', 'Q', 'Q', 'J', 'A'),483)
  )

  test("parse") {
    assert(raw.split("\n").toList.map(parseHand) == parsed)
  }

  test("part1") {
    assert(part1(parsed) == 6440)
  }

  test("evaluate") {
    assert(parsed.map(hand => evaluate(hand.cards)) == List(
      "One pair",
      "Three of a kind",
      "Two pairs",
      "Two pairs",
      "Three of a kind")
    )
  }

  test("evaluate2") {
    assert(parsed.map(hand => evaluate2(hand.cards)) == List(
      "One pair",
      "Four of a kind",
      "Two pairs",
      "Four of a kind",
      "Four of a kind")
    )
  }

  test("part2") {
    assert(part2(parsed) == 5905)
  }
}
