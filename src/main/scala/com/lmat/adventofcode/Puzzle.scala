package com.lmat.adventofcode

trait Puzzle[I1, P1, I2, P2] {
  def parse1(resource: String): I1
  def parse2(resource: String): I2

  def part1(input: I1): P1
  def part2(input: I2): P2

  def solve(resource: String): (P1, P2) = {
    val input1  = parse1(resource)
    val input2  = parse2(resource)
    val result1 = part1(input1)
    val result2 = part2(input2)
    (result1, result2)
  }
}

trait SimplePuzzle[I, P1, P2] extends Puzzle[I, P1, I, P2] {
  def parse(resource: String): I

  override def parse1(resource: String): I = parse(resource)
  override def parse2(resource: String): I = parse(resource)

  override def solve(resource: String): (P1, P2) = {
    val input  = parse(resource)
    val result1 = part1(input)
    val result2 = part2(input)
    (result1, result2)
  }
}
