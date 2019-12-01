package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.year2015.Day24._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day24Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val packages = Set(1, 2, 3, 4, 5, 7, 8, 9, 10, 11)

  test("Day24 - Parts") {
    assert(separatePackages(packages, 3).map(entanglement).min == 99)
    assert(separatePackages(packages, 4).map(entanglement).min == 44)
  }

  test("Day24 - Groups") {
    assert(separatePackages(packages, 3).toList == List(Set(9, 11)))
    assert(separatePackages(packages, 4).toList == List(Set(5, 10), Set(7, 8), Set(11, 4)))
  }
}
