package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimplePuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day12 extends SimplePuzzle[Map[Int, Set[Int]], Int, Int] {
  type ProgramMap = Map[Int, Set[Int]]

  override def parse(resource: String): ProgramMap = parseProgramMap(readResource(resource))

  def parseProgramMap(rows: Seq[String]): ProgramMap = {
    case class Program(id: Int, connections: Set[Int])

    def parseProgram(line: String): Option[Program] = {
      val regex = "(.*) <-> (.*)".r
      line match {
        case regex(id, list) => Some(Program(id.toInt, list.split(", ").map(_.toInt).toSet))
        case _               => None
      }
    }

    rows.flatMap(parseProgram).groupBy(_.id).mapValues(_.head.connections)
  }

  override def part1(programMap: ProgramMap): Int = connectedTo(programMap)(0).size

  def connectedTo(programMap: ProgramMap)(id: Int): Set[Int] = {
    @tailrec
    def connectedTo(connected: Set[Int], toCheck: Set[Int]): Set[Int] =
      if (toCheck.isEmpty) connected
      else connectedTo(connected + toCheck.head, (programMap(toCheck.head) -- connected) ++ toCheck.tail)

    connectedTo(Set(), Set(id))
  }

  override def part2(programMap: ProgramMap): Int = groups(programMap).size

  def groups(programMap: ProgramMap): Set[Set[Int]] =
    programMap.keySet.map(id => connectedTo(programMap)(id))
}
