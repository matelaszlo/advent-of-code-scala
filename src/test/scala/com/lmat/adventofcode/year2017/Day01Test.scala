package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day01.solveCaptcha
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day01Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val captchas =
    Table(
      ("captcha",   "result"),
      ("1122",      3),
      ("1111",      4),
      ("1234",      0),
      ("91212129",  9),
    )

  test("Day01 - Part 1") {
    forAll(captchas) { (captcha, result) =>
      assert(solveCaptcha(1)(captcha) == result)
    }
  }

  val captchas2 =
    Table(
      ("captcha",   "result"),
      ("1212",      6),
      ("1221",      0),
      ("123425",    4),
      ("123123",    12),
      ("12131415",  4)
    )

  test("Day01 - Part 2") {
    forAll(captchas2) { (captcha, result) =>
      assert(solveCaptcha(captcha.length / 2)(captcha) == result)
    }
  }
}
