package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day24 extends SimpleCommonPuzzle[Set[Int], Long, Long]{
  override def parse(resource: String): Set[Int] = readResource(resource).map(_.toInt).toSet

  override def part1(packages: Set[Int]): Long =
    separatePackages(packages, 3).map(entanglement).min

  override def part2(packages: Set[Int]): Long =
    separatePackages(packages, 4).map(entanglement).min

  /**
    * Generate an infinite Stream of package subsets
    * That have a sum that is nth of the sum of all the packages
    * Where the rest of the packages are divisible to n-1 groups that have the same size as the original package
    * And where the group is minimally sized
    */
  def separatePackages(packages: Set[Int], n: Int): LazyList[Set[Int]] = {
    val groupOneCandidates = candidates(packages, n).filter(candidate => check(packages -- candidate, n - 1, packages.sum / n))
    groupOneCandidates.takeWhile(_.size == groupOneCandidates.head.size)
  }

  /**
    * Generate an infinite Stream of package subsets
    * That have a sum that is nth of the sum of all the packages
    * In increasing size
    */
  def candidates(packages: Set[Int], n: Int): LazyList[Set[Int]] =
    LazyList.from(1).flatMap(i => packages.subsets(i).filter(_.sum == packages.sum / n))

  /**
    * Check if the packages are divisible to n groups with equal sums
    */
  def check(packages: Set[Int], n: Int, sum: Int): Boolean =
    if(n == 1 && packages.sum == sum) true
    else if( n == 1) false
    else candidates(packages, n).exists(candidate => check(packages -- candidate, n - 1, sum))

  /**
    * We need to promote our packages to Long as the product quickly scales out of the Int range
    */
  def entanglement(packages: Set[Int]): Long =
    packages.map(_.toLong).product
}
