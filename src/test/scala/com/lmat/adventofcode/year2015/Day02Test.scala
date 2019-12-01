package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day02._
import com.lmat.adventofcode.year2015.Day02Definitions.Box
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day02Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val boxes =
    Table(
      ("box",         "wrapping", "ribbon"),
      (Box(2, 3, 4),  58,         34),
      (Box(1, 1, 10), 43,         14)
    )

  test("Day02 - Part 1") {
    forAll(boxes) { (box, w, _) =>
      assert(wrapping(box) == w)
    }
  }

  test("Day02 - Part 2") {
    forAll(boxes) { (box, _, r) =>
      assert(ribbon(box) == r)
    }
  }
}
