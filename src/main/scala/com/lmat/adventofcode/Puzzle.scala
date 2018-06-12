package com.lmat.adventofcode

/**
  * The most generic and versatile puzzle interface
  */
trait Puzzle[R1, I1, P1, R2, I2, P2] {
  def parse1(resource: String): R1
  def parse2(resource: String): R2

  def preProcess1(raw: R1): I1
  def preProcess2(raw: R2): I2

  def part1(input: I1): P1
  def part2(input: I2): P2

  def solve(resource: String): (P1, P2) = {
    val result1 = part1(preProcess1(parse1(resource)))
    val result2 = part2(preProcess2(parse2(resource)))
    (result1, result2)
  }
}

/**
  * A puzzle with differently parsed inputs but no pre-processing
  * With no pre-processing required
  */
trait SimpleMultiPuzzle[I1, P1, I2, P2] extends Puzzle[I1, I1, P1, I2, I2, P2] {
  override def preProcess1(raw: I1): I1 = raw
  override def preProcess2(raw: I2): I2 = raw
}

/**
  * A puzzle with a simple input that is applicable for both parts
  * The input is pre-processed as there is a computation heavy common step
  */
trait CommonPuzzle[R, I, P1, P2] extends Puzzle[R, I, P1, R, I, P2] {
  def parse(resource: String): R
  def preProcess(raw: R): I

  override def parse1(resource: String): R = parse(resource)
  override def parse2(resource: String): R = parse(resource)

  override def preProcess1(raw: R): I = preProcess(raw)
  override def preProcess2(raw: R): I = preProcess(raw)

  override def solve(resource: String): (P1, P2) = {
    val input  = preProcess(parse(resource))
    val result1 = part1(input)
    val result2 = part2(input)
    (result1, result2)
  }
}

/**
  * A Puzzle with a simple input that is applicable for both parts
  * With no pre-processing required
  */
trait SimpleCommonPuzzle[I, P1, P2] extends CommonPuzzle[I, I, P1, P2] {
  override def preProcess(raw: I): I = raw
}
