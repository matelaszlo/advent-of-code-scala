package com.lmat.adventofcode

trait Puzzle[I, P1, P2] {
  def parse(resource: String): I

  def part1(input: I): P1
  def part2(input: I): P2

  def solve(resource: String): (P1, P2) = {
    val input   = parse(resource)
    val result1 = part1(input)
    val result2 = part2(input)
    (result1, result2)
  }
}
