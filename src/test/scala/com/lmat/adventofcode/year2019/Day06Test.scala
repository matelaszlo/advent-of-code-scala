package com.lmat.adventofcode.year2019

import org.scalatest.funsuite.AnyFunSuite
import com.lmat.adventofcode.year2019.Day06._
import com.lmat.adventofcode.year2019.Day06Definitions._

class Day06Test extends AnyFunSuite {

  val raw1 =
  """COM)B
    |B)C
    |C)D
    |D)E
    |E)F
    |B)G
    |G)H
    |D)I
    |E)J
    |J)K
    |K)L""".stripMargin

  val raw2 =
    """COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L
      |K)YOU
      |I)SAN""".stripMargin

  val parsed2 =
    OrbitInfo(
      Map(
        "E"   -> List("F", "J"),
        "J"   -> List("K"),
        "COM" -> List("B"),
        "I"   -> List("SAN"),
        "G"   -> List("H"),
        "B"   -> List("C", "G"),
        "C"   -> List("D"),
        "K"   -> List("L", "YOU"),
        "D"   -> List("E", "I")),
      Map(
        "E"   -> "D",
        "J"   -> "E",
        "F"   -> "E",
        "I"   -> "D",
        "SAN" -> "I",
        "G"   -> "B",
        "L"   -> "K",
        "B"   -> "COM",
        "YOU" -> "K",
        "C"   -> "B",
        "H"   -> "G",
        "K"   -> "J",
        "D"   -> "C"))


  test("preprocess"){
    assert(preProcess(raw1.split('\n').toList.map(parseOrbit)) == parsed1)
    assert(preProcess(raw2.split('\n').toList.map(parseOrbit)) == parsed2)
  }

  lazy val parsed1 =
    OrbitInfo(
      Map(
        "E"   -> List("F", "J"),
        "J"   -> List("K"),
        "COM" -> List("B"),
        "G"   -> List("H"),
        "B"   -> List("C", "G"),
        "C"   -> List("D"),
        "K"   -> List("L"),
        "D"   -> List("E", "I")),
      Map(
        "E" -> "D",
        "J" -> "E",
        "F" -> "E",
        "I" -> "D",
        "G" -> "B",
        "L" -> "K",
        "B" -> "COM",
        "C" -> "B",
        "H" -> "G",
        "K" -> "J",
        "D" -> "C"))


  test("orbitCountMap") {
    val result = Map(
      "E"   -> 4,
      "J"   -> 5,
      "I"   -> 4,
      "G"   -> 2,
      "L"   -> 7,
      "B"   -> 1,
      "C"   -> 2,
      "H"   -> 3,
      "K"   -> 6,
      "D"   -> 3,
      "COM" -> 0,
      "F"   -> 5)

    assert(orbitCountMap(parsed1) == result)
    assert(part1(parsed1) == 42)
  }

  test("orbitalTransfers") {
    assert(parents(parsed2)("YOU") == List("K", "J", "E", "D", "C", "B", "COM"))
    assert(parents(parsed2)("SAN") == List("I", "D", "C", "B", "COM"))
    assert(orbitalTransfers(parsed2)("YOU", "SAN") == 4)
  }
}
