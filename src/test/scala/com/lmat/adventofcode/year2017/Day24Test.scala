package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day24._
import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2017.Day24Definitions._

class Day24Test extends AnyFunSuite {

  test("End Connection") {
    assert(Bridge(Seq(), 0).endConnection == 0)
    assert(Bridge(Seq(Component(Seq(0, 1))), 0).endConnection == 1)
    assert(Bridge(Seq(Component(Seq(1, 0))), 0).endConnection == 1)
    assert(Bridge(Seq(Component(Seq(1, 0)), Component(Seq(2, 1))), 0).endConnection == 2)
    assert(Bridge(Seq(Component(Seq(1, 0)), Component(Seq(1, 1))), 0).endConnection == 1)
    assert(Bridge(Seq(Component(Seq(1, 0)), Component(Seq(1, 1)), Component(Seq(2, 1))), 0).endConnection == 2)
  }

  val components = Seq(
    Component(0, 2),
    Component(2, 2),
    Component(2, 3),
    Component(3, 4),
    Component(3, 5),
    Component(0, 1),
    Component(10, 1),
    Component(9, 10),
  )

  val validBridges = Seq(
    Bridge(Seq(Component(0, 1)), 0),
    Bridge(Seq(Component(0, 1), Component(10, 1)), 0),
    Bridge(Seq(Component(0, 1), Component(10, 1), Component(9, 10)), 0),

    Bridge(Seq(Component(0, 2)), 0),
    Bridge(Seq(Component(0, 2), Component(2, 3)), 0),
    Bridge(Seq(Component(0, 2), Component(2, 3), Component(3, 4)), 0),
    Bridge(Seq(Component(0, 2), Component(2, 3), Component(3, 5)), 0),
    Bridge(Seq(Component(0, 2), Component(2, 3)), 0),

    Bridge(Seq(Component(0, 2), Component(2, 2)), 0),
    Bridge(Seq(Component(0, 2), Component(2, 2), Component(2, 3)), 0),
    Bridge(Seq(Component(0, 2), Component(2, 2), Component(2, 3), Component(3, 4)), 0),
    Bridge(Seq(Component(0, 2), Component(2, 2), Component(2, 3), Component(3, 5)), 0),
  )

  test("Build Bridges") {
    assert(buildBridges(initialBridges(components, 0), components).toSet == validBridges.toSet)
  }

  test("Part 1") {
    assert(part1(validBridges) == 31)
  }

  test("Part 2") {
    assert(part2(validBridges) == 19)
  }
}
