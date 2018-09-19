package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day11._
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day11Test extends FunSuite with TableDrivenPropertyChecks {

  val sequence = Seq(
    "ax",
    "ay",
    "az",
    "ba",
    "bb",
    "bc"
  )

  test("Day11 - Increment") {
    assert(Stream.iterate("ax")(increment).take(6) == sequence)
  }

  val passwords =
    Table(
      ("password", "valid"),
      ("hijklmmn", false),
      ("abbceffg", false),
      ("abbceabc", false),
      ("abcdffaa", true),
      ("ghjaabcc", true)
    )

  test("Day11 - Validity") {
    forAll(passwords) { (password, valid) =>
      assert(isValid(password) == valid)
    }
  }

  val passwords2 =
    Table(
      ("password", "next"),
      ("abcdefgh", "abcdffaa"),
      ("ghijklmn", "ghjaabcc")
    )

  test("Day11 - Next") {
    forAll(passwords2) { (password, next) =>
      assert(nextPassword(password) == next)
    }
  }
}
