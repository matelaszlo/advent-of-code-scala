package com.lmat.adventofcode.year2023

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2023.Day12._
import com.lmat.adventofcode.year2023.Day12Definitions._

class Day12Test extends AnyFunSuite {
  val raw =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1""".stripMargin

  val springs: List[Springs] = List(
    (List(Unknown, Unknown, Unknown, Operational, Damaged, Damaged, Damaged), List(1, 1, 3)),
    (List(Operational, Unknown, Unknown, Operational, Operational, Unknown, Unknown, Operational, Operational, Operational, Unknown, Damaged, Damaged, Operational), List(1, 1, 3)),
    (List(Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown, Damaged, Unknown), List(1, 3, 1, 6)),
    (List(Unknown, Unknown, Unknown, Unknown, Operational, Damaged, Operational, Operational, Operational, Damaged, Operational, Operational, Operational), List(4, 1, 1)),
    (List(Unknown, Unknown, Unknown, Unknown, Operational, Damaged, Damaged, Damaged, Damaged, Damaged, Damaged, Operational, Operational, Damaged, Damaged, Damaged, Damaged, Damaged, Operational), List(1, 6, 5)),
    (List(Unknown, Damaged, Damaged, Damaged, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown, Unknown), List(3, 2, 1))
  )

  test("parse") {
    assert(raw.split("\n").toList.map(parseSprings) == springs)
  }

  test("arrangements") {
    assert(arrangements(parseSprings("???.### 1,1,3")) == 1)
    assert(arrangements(parseSprings(".??..??...?##. 1,1,3")) == 4)
    assert(arrangements(parseSprings("?#?#?#?#?#?#?#? 1,3,1,6")) == 1)
    assert(arrangements(parseSprings("????.#...#... 4,1,1")) == 1)
    assert(arrangements(parseSprings("????.######..#####. 1,6,5")) == 4)
    assert(arrangements(parseSprings("?###???????? 3,2,1")) == 10)

    assert(arrangements(parseSprings("???? 1")) == 4)
    assert(arrangements(parseSprings("??? 1,1")) == 1)
    assert(arrangements(parseSprings("???? 1,1")) == 3)
    assert(arrangements(parseSprings("????? 1,1")) == 6)
    assert(arrangements(parseSprings("?????? 1,1")) == 10)
    assert(arrangements(parseSprings("??????? 1,1")) == 15)
    assert(arrangements(parseSprings("???? 2,1")) == 1)
    assert(arrangements(parseSprings("????? 2,1")) == 3)
    assert(arrangements(parseSprings("?????? 2,1")) == 6)
    assert(arrangements(parseSprings("??????? 2,1")) == 10)
    assert(arrangements(parseSprings("???????? 2,1")) == 15)
    assert(arrangements(parseSprings("????? 1,1,1")) == 1)
    assert(arrangements(parseSprings("?????? 1,1,1")) == 4)
    assert(arrangements(parseSprings("??????? 1,1,1")) == 10)
    assert(arrangements(parseSprings("???????? 1,1,1")) == 20)
    assert(arrangements(parseSprings("????????? 1,1,1")) == 35)
    assert(arrangements(parseSprings("?????????? 1,1,1")) == 56)
    assert(arrangements(parseSprings("### 3")) == 1)
    assert(arrangements(parseSprings("... 3")) == 0)
    assert(arrangements(parseSprings("??? 3")) == 1)

    assert(arrangements(parseSprings("###.### 3")) == 0)
    assert(arrangements(parseSprings("??#???? 5")) == 3)
    assert(arrangements(parseSprings("??#???#? 5")) == 1)
    assert(arrangements(parseSprings(".#.#.#.#.###### 1,3,1,6")) == 0)
    assert(arrangements(parseSprings(".##.?#??.#.?# 2,1,1,1")) == 1)
    assert(arrangements(parseSprings("#???#?#?.?##?#?.#. 7,5,1")) == 2)
    assert(arrangements(parseSprings("?.??#???????#?# 2,9")) == 1)
    assert(arrangements(parseSprings("???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3")) == 1)
    assert(arrangements(parseSprings(".??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##. 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3")) == 16384)
  }

  test("part1") {
    assert(part1(springs) == 21)
  }

  test("part2") {
    assert(part2(springs) == 525152)
  }
}
