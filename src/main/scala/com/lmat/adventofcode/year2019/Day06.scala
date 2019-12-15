package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.adventofcode.year2019.Day06Definitions._
import com.lmat.util.Files.readResource

import scala.collection.immutable.Nil

object Day06Definitions {
  case class Orbit(center: String, orbiting: String)
  case class OrbitInfo(orbitMap: Map[String, List[String]], parentMap: Map[String, String])
}

object Day06 extends CommonPuzzle[List[Orbit], OrbitInfo, Int, Int] {
  override def parse(resource: String): List[Orbit] =
    readResource(resource).toList.map(parseOrbit)

  def parseOrbit(row: String): Orbit = {
    val c :: o :: Nil = row.split(')').toList
    Orbit(c, o)
  }

  override def preProcess(orbits: List[Orbit]): OrbitInfo = {
    val orbitMap = orbits.groupBy(_.center).view.mapValues(_.map(_.orbiting)).toMap
    val parentMap = orbits.groupBy(_.orbiting).view.mapValues(_.head.center).toMap
    OrbitInfo(orbitMap, parentMap)
  }

  override def part1(orbitInfo: OrbitInfo): Int =
    orbitCountMap(orbitInfo).values.sum

  // We can get away with not building the actual tree by starting from the center of mass and
  // recursively adding the children into an orbitCountMap
  def orbitCountMap(orbitInfo: OrbitInfo): Map[String, Int] = {
    val centerOfMass = "COM"

    def next(map: Map[String, Int], current: List[String]): (Map[String, Int], List[String]) = {
      def count(c: String): Int =
        if(c == centerOfMass) 0
        else map(orbitInfo.parentMap(c)) + 1

      val m = map ++ current.map(c => (c, count(c))).toMap
      val n = current.flatMap(c => orbitInfo.orbitMap.getOrElse(c, List()))
      (m, n)
    }

    LazyList.iterate((Map[String, Int](), List(centerOfMass))){ case (map, current) => next(map, current) }
      .find{ case (_, current) => current.isEmpty }
      .map{ case (map, _)      => map }.get
  }

  override def part2(orbitInfo: OrbitInfo): Int =
    orbitalTransfers(orbitInfo)("YOU", "SAN")

  // We can get away with not building the actual tree by listing the parents from both sides and finding the closest matching one
  def orbitalTransfers(orbitInfo: OrbitInfo)(from: String, to: String): Int = {
    def countTransfers(parents: List[String])(c: String): Int =
      parents.zipWithIndex.find{case (p, _) => p == c}.map{ case (_, i) => i }.get

    val fromParents = parents(orbitInfo)(from)
    val toParents = parents(orbitInfo)(to)

    toParents.find(fromParents.contains)
      .map(c => countTransfers(fromParents)(c) + countTransfers(toParents)(c)).get
  }

  def parents(orbitInfo: OrbitInfo)(from: String): List[String] =
    LazyList.iterate(Option(from))(_.flatMap(c => orbitInfo.parentMap.get(c))).drop(1).takeWhile(_.isDefined).map(_.get).toList
}
