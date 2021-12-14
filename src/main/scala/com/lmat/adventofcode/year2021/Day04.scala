package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2021.Day04Definitions._
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix

object Day04Definitions {
  case class Cell(number: Int, marked: Boolean)
  type Board = Matrix[Cell]
  case class Bingo(
    n         : Int,
    playing   : List[Board],
    remaining : List[Int],
    called    : List[Int],
    winners   : List[Board]
  )
}

object Day04 extends SimpleCommonPuzzle[Bingo, Int, Int] {
  override def parse(resource: String): Bingo =
    parseBingo(5)(readResource(resource).toList)

  def parseBingo(n: Int)(rows: List[String]): Bingo = {
    val first :: rest = rows
    val numbers = parseNumbers(first)
    val boards = rest.sliding(n + 1, n + 1).map(block => parseBoard(block.drop(1))).toList
    Bingo(n, boards, numbers, List.empty, List.empty)
  }

  def parseBoard(rows: List[String]): Board =
    Matrix(rows.map(_.split(" ").flatMap(_.toIntOption).map(Cell(_, false)).toVector).toVector)

  def parseNumbers(row: String): List[Int] =
    row.split(",").flatMap(_.toIntOption).toList

  override def part1(bingo: Bingo): Int = {
    val winState: Bingo = play(bingo).filter(hasWinner).head
    score(winState.winners.head, winState.called.last)
  }

  override def part2(bingo: Bingo): Int = {
    val winState: Bingo = play(bingo).last
    score(winState.winners.last, winState.called.last)
  }

  // We play Bingo until
  // - All Boards are winners (no more boards remain in the playing pile)
  // - We run out of numbers
  def play(bingo: Bingo): LazyList[Bingo] =
    LazyList.iterate(Option(bingo)) {
      case Some(b) => playRound(b)
      case None => None
    }.takeWhile(_.nonEmpty).map(_.get)

  def playRound(bingo: Bingo): Option[Bingo] =
    bingo.remaining.headOption.flatMap(current =>
      if (bingo.playing.isEmpty) None
      else {
        val newBoards = bingo.playing.map(mark(current))
        val (newWinning, newPlaying) = newBoards.partition(isWinning)
        Some(bingo.copy(playing = newPlaying, winners = bingo.winners ++ newWinning, called = bingo.called :+ current, remaining = bingo.remaining.tail))
      })

  def mark(called: Int)(board: Board): Board =
    board.map(cell => if(cell.number == called) cell.copy(marked = true) else cell)

  def hasWinner(bingo: Bingo): Boolean =
    bingo.winners.nonEmpty

  def isWinning(board: Board): Boolean =
    board.rows.exists(_.forall(_.marked)) || board.columns.exists(_.forall(_.marked))

  def score(board: Board, called: Int): Int =
    board.rows.flatten.filterNot(_.marked).map(_.number).sum * called
}
