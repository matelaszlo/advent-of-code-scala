package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.CommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day12 extends CommonPuzzle[List[(String, String)], Map[String, Set[String]], Int, Int] {
  override def parse(resource: String): List[(String, String)] =
    readResource(resource).flatMap(parseRow).toList

  def parseRow(row: String): Option[(String, String)] = {
    val components = row.split("-")
    components.headOption.flatMap(from => components.lift(1).map(to => (from, to)))
  }

  override def preProcess(raw: List[(String, String)]): Map[String, Set[String]] = {
    val map1 = raw.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
    val map2 = raw.groupBy(_._2).view.mapValues(_.map(_._1).toSet).toMap
    merge(map1, map2)
  }

  def merge[A](a: Map[A, Set[A]], b: Map[A, Set[A]]): Map[A, Set[A]] =
    (a.keySet ++ b.keySet).map(key => (key, a.getOrElse(key, Set.empty) ++ b.getOrElse(key, Set.empty))).toMap

  override def part1(caves: Map[String, Set[String]]): Int =
    allPaths(caves, noDuplicateVisits).size

  override def part2(caves: Map[String, Set[String]]): Int =
    allPaths(caves, oneDuplicateVisit).size

  def allPaths(caves: Map[String, Set[String]], smallCaveRule: (List[String], String) => Boolean): Set[List[String]] = {
    def generate(incompletePath: List[String]): Set[List[String]] = {
      val continuations = incompletePath.lastOption.flatMap(caves.get).getOrElse(Set.empty[String])
      val (small, big) = continuations.filterNot(_ == "start").partition(isLower)
      (big ++ small.filter(smallCaveRule(incompletePath, _))).map(incompletePath :+ _)
    }

    @tailrec
    def loop(completePaths: Set[List[String]], incompletePaths: Set[List[String]]): Set[List[String]] =
      if (incompletePaths.isEmpty) completePaths
      else {
        val newPaths = incompletePaths.flatMap(generate)
        val (newCompletePaths, newIncompletePaths) = newPaths.partition(_.lastOption.contains("end"))
        loop(completePaths ++ newCompletePaths, newIncompletePaths)
      }

    loop(Set(), Set(List("start")))
  }

  def duplicateVisits(path: List[String], cave: String): Boolean =
    path.contains(cave)

  def noDuplicateVisits(path: List[String], cave: String): Boolean =
    !duplicateVisits(path, cave)

  def oneDuplicateVisit(path: List[String], cave: String): Boolean =
    !(containsDuplicate(path.filter(isLower)) && duplicateVisits(path, cave))

  def isLower(raw: String): Boolean =
    raw == raw.toLowerCase

  def containsDuplicate[A](list: List[A]): Boolean =
    list.size != list.toSet.size
}
