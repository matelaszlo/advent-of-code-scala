package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day10 extends SimpleCommonPuzzle[List[String], Int, Long]{
  override def parse(resource: String): List[String] =
    readResource(resource).toList

  override def part1(lines: List[String]): Int =
    lines.flatMap(corrupted).map(scoreIllegal).sum

  override def part2(lines: List[String]): Long = {
    val scores = lines.filter(line => corrupted(line).isEmpty).map(autoComplete).map(scoreIncomplete)
    middle(scores)
  }

  // Algorithm: We keep track of opened chunks in essentially a stack, removing each layer when finding a valid closing character
  // On the first illegal character we terminate and return it
  def corrupted(line: String): Option[Char] = {
    @tailrec
    def loop(opened: List[Char], remaining: List[Char]): Option[Char] =
      remaining match {
        case Nil               => None
        case ::(current, next) =>
          if (isOpening(current)) loop(opened :+ current, next)
          else if (isClosing(current) && opened.nonEmpty && matches(opened.last, current)) loop(opened.dropRight(1), next)
          else Some(current)
      }
    loop(List(), line.toCharArray.toList)
  }

  // Invariant: The line is not corrupted
  // Algorithm: We go through the line maintaining the stack of opened chunks
  // Whatever remains at the end we have to close in reverse order
  def autoComplete(line: String): String =
    line.toCharArray
      .foldLeft(List.empty[Char])((opened, current) => if (isOpening(current)) opened :+ current else opened.dropRight(1))
      .reverse.map(close)
      .mkString("")

  // Invariant: The list has an odd number of scores
  def middle[A: Ordering](scores: List[A]): A = {
    val sorted: List[A] = scores.sorted
    sorted(scores.size / 2)
  }

  def scoreIncomplete(completion: String): Long =
    completion.toCharArray.foldLeft(0L)((score, current) => score * 5 + scoreIncomplete(current))

  def scoreIllegal(char: Char): Int = char match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
    case _   => 0
  }

  def scoreIncomplete(char: Char): Int = char match {
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
    case _   => 0
  }

  def isOpening(char: Char): Boolean =
    Set('(', '[', '{', '<').contains(char)

  def isClosing(char: Char): Boolean =
    Set(')', ']', '}', '>').contains(char)

  def matches(opening: Char, closing: Char): Boolean =
    Set(('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')).contains((opening, closing))

  def close(char: Char): Char = char match {
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
    case _   => ' '
  }
}
