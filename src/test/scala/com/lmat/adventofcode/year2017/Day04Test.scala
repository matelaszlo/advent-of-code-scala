package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day04.{hasNoAnagrams, hasOnlyUniqueWords}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day04Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val examples =
    Table(
      ("input",           "isValid"),
      ("aa bb cc dd ee",  true),
      ("aa bb cc dd aa",  false),
      ("aa bb cc dd aaa", true)
    )

  test("Day04 - Part 1") {
    forAll(examples) { (input, isValid) =>
      assert(hasOnlyUniqueWords(input.split("\\s+").toSeq) == isValid)
    }
  }

  val examples2 =
    Table(
      ("input",                    "isValid"),
      ("abcde fghij",              true),
      ("abcde xyz ecdab",          false),
      ("a ab abc abd abf abj",     true),
      ("iiii oiii ooii oooi oooo", true),
      ("oiii ioii iioi iiio",      false)
    )

  test("Day04 - Part 2") {
    forAll(examples2) { (input, isValid) =>
      assert(hasNoAnagrams(input.split("\\s+").toSeq) == isValid)
    }
  }
}
