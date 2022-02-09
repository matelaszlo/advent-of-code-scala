package com.lmat.adventofcode.year2021

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2021.Day12._

class Day12Test extends AnyFunSuite {
  val small =
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end""".stripMargin

  val medium =
    """dc-end
      |HN-start
      |start-kj
      |dc-start
      |dc-HN
      |LN-dc
      |HN-end
      |kj-sa
      |kj-HN
      |kj-dc""".stripMargin

  val large =
    """fs-end
      |he-DX
      |fs-he
      |start-DX
      |pj-DX
      |end-zg
      |zg-sl
      |zg-pj
      |pj-he
      |RW-he
      |fs-DX
      |pj-RW
      |zg-RW
      |start-pj
      |he-WI
      |zg-he
      |pj-fs
      |start-RW""".stripMargin

  val smallPaths =
    """start,A,b,A,c,A,end
      |start,A,b,A,end
      |start,A,b,end
      |start,A,c,A,b,A,end
      |start,A,c,A,b,end
      |start,A,c,A,end
      |start,A,end
      |start,b,A,c,A,end
      |start,b,A,end
      |start,b,end""".stripMargin

  val mediumPaths =
    """start,HN,dc,HN,end
      |start,HN,dc,HN,kj,HN,end
      |start,HN,dc,end
      |start,HN,dc,kj,HN,end
      |start,HN,end
      |start,HN,kj,HN,dc,HN,end
      |start,HN,kj,HN,dc,end
      |start,HN,kj,HN,end
      |start,HN,kj,dc,HN,end
      |start,HN,kj,dc,end
      |start,dc,HN,end
      |start,dc,HN,kj,HN,end
      |start,dc,end
      |start,dc,kj,HN,end
      |start,kj,HN,dc,HN,end
      |start,kj,HN,dc,end
      |start,kj,HN,end
      |start,kj,dc,HN,end
      |start,kj,dc,end""".stripMargin

  val smallPaths2 =
    """start,A,b,A,b,A,c,A,end
      |start,A,b,A,b,A,end
      |start,A,b,A,b,end
      |start,A,b,A,c,A,b,A,end
      |start,A,b,A,c,A,b,end
      |start,A,b,A,c,A,c,A,end
      |start,A,b,A,c,A,end
      |start,A,b,A,end
      |start,A,b,d,b,A,c,A,end
      |start,A,b,d,b,A,end
      |start,A,b,d,b,end
      |start,A,b,end
      |start,A,c,A,b,A,b,A,end
      |start,A,c,A,b,A,b,end
      |start,A,c,A,b,A,c,A,end
      |start,A,c,A,b,A,end
      |start,A,c,A,b,d,b,A,end
      |start,A,c,A,b,d,b,end
      |start,A,c,A,b,end
      |start,A,c,A,c,A,b,A,end
      |start,A,c,A,c,A,b,end
      |start,A,c,A,c,A,end
      |start,A,c,A,end
      |start,A,end
      |start,b,A,b,A,c,A,end
      |start,b,A,b,A,end
      |start,b,A,b,end
      |start,b,A,c,A,b,A,end
      |start,b,A,c,A,b,end
      |start,b,A,c,A,c,A,end
      |start,b,A,c,A,end
      |start,b,A,end
      |start,b,d,b,A,c,A,end
      |start,b,d,b,A,end
      |start,b,d,b,end
      |start,b,end""".stripMargin

  def parseInput(raw: String): Map[String, Set[String]] =
    preProcess(raw.split("\n").flatMap(parseRow).toList)

  def parseResult(raw: String): Set[List[String]] =
    raw.split("\n").map(_.split(",").toList).toSet

  test("allPaths") {
    assert(allPaths(parseInput(small), noDuplicateVisits) == parseResult(smallPaths))
    assert(allPaths(parseInput(medium), noDuplicateVisits) == parseResult(mediumPaths))
    assert(allPaths(parseInput(small), oneDuplicateVisit) == parseResult(smallPaths2))
  }

  test("part1") {
    assert(part1(parseInput(small)) == 10)
    assert(part1(parseInput(medium)) == 19)
    assert(part1(parseInput(large)) == 226)
  }

  test("part2") {
    assert(part2(parseInput(small)) == 36)
    assert(part2(parseInput(medium)) == 103)
    assert(part2(parseInput(large)) == 3509)
  }
}
